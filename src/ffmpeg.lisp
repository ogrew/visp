(in-package :visp)

(defun validate-output-path (output)
  "Validate output path before ffmpeg execution"
  (let* ((output-pathname (pathname output))
         (output-dir (uiop:pathname-directory-pathname output-pathname)))
    ;; Check directory existence (resolve relative paths from current directory)
    (unless (uiop:directory-exists-p output-dir)
      (format t "~a Output directory does not exist: ~a~%" 
              (log-tag "error") (namestring output-dir))
      (uiop:quit 1))
    
    ;; Basic output filename validation
    (let ((filename (file-namestring output-pathname)))
      (when (string= filename "")
        (format t "~a Invalid output filename: ~a~%" 
                (log-tag "error") output)
        (uiop:quit 1)))))

(defun cleanup-partial-output (output)
  "Clean up partially created output file if it exists and is empty/corrupted"
  (when (probe-file output)
    (let ((file-size (with-open-file (stream output :direction :input 
                                             :if-does-not-exist nil)
                       (when stream (file-length stream)))))
      ;; Delete empty or very small files (header-only, etc.)
      (when (and file-size (< file-size 1024))
        (delete-file output)
        (format t "~a Cleaned up incomplete output file: ~a~%" 
                (log-tag "info") output)))))

(defun run-cmd (cmd output dry-run)
  ;; Pre-execution validation
  (validate-output-path output)
  
  (when (probe-file output)
    (format t "~a Output file '~a' already exists. It will be overwritten.~%"
            (log-tag "warn") output))

  (if dry-run
      (progn
        (format t "~a Planned output file: ~a~%" (log-tag "info") output)
        (format t "~a Command: ~{~a ~}~%" (log-tag "dry-run") cmd))
      (progn
        (format t "~a Running: ~{~a ~}~%" (log-tag "info") cmd)
        ;; Error handling with cleanup
        (handler-case
            (uiop:run-program cmd :output t :error-output t)
          (error (e)
            ;; Basic error handling (to be enhanced later)
            (cleanup-partial-output output)
            (error e))))))

(defun build-gif-cmd (opts output fps)
  "Construct the ffmpeg command list for GIF mode using input filename and target fps."
  (let* ((input (visp-options-input opts))
         (cmd (list "ffmpeg" "-i" input))
         (half-fps (/ fps 2.0))
         (scale-str (format nil "scale=~a" +gif-scale+))
         (fps-str (format nil "fps=~2,2f" half-fps))
         (filter-complex
           (format nil
             "[0:v] ~a,~a,split [a][b];[a] palettegen=stats_mode=single [p];[b][p] paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle:new=1"
              fps-str scale-str)))

    (setf cmd (append cmd (list "-filter_complex" filter-complex)))
    (setf cmd (append cmd (list "-y" output)))
    cmd))

(defun build-concat-filter (files video-info audio-enabled-p)
  "Generate filter_complex string for ffmpeg concat."
  (let* ((fps (getf video-info :fps))
         (width (getf video-info :width))
         (height (getf video-info :height))
         (v-labels '())
         (a-labels '())
         (v-lines '())
         (a-lines '()))
    (loop for f in files
          for idx from 0
          do
            (let ((v-out (format nil "v~a" idx))
                  (a-out (format nil "a~a" idx)))
              (push v-out v-labels)
              (push (format nil "[~a:v:0]~a[~a];"
                            idx
                            (if (zerop idx)
                                "setpts=PTS-STARTPTS"
                                (format nil "fps=~a,scale=~a:~a,setpts=PTS-STARTPTS"
                                        (format nil "~,2f" fps) width height))
                            v-out)
                    v-lines)
              (when audio-enabled-p
                (push a-out a-labels)
                (push (format nil "[~a:a:0]asetpts=PTS-STARTPTS[~a];" idx a-out)
                      a-lines))))
    (let ((concat-part (format nil "~{[~a]~}~{[~a]~}concat=n=~a:v=1:a=~a[outv]~a"
                                (reverse v-labels)
                                (if audio-enabled-p (reverse a-labels) '())
                                (length files)
                                (if audio-enabled-p 1 0)
                                (if audio-enabled-p "[outa]" ""))))
      (concatenate 'string
                   (apply #'concatenate 'string (reverse v-lines))
                   (apply #'concatenate 'string (reverse a-lines))
                   concat-part))))

(defun build-merge-cmd (opts output)
  "Build ffmpeg command for --merge mode."
  (let* ((files (visp-options-merge-files opts))
         (input-args (mapcan (lambda (f) (list "-i" f)) files))
         (video-infos (mapcar #'get-video-info files))
         (first-video-info (car video-infos))
         (fps (getf first-video-info :fps))
         (width (getf first-video-info :width))
         (height (getf first-video-info :height)))
    
    ;; Ensure video metadata is valid
    (unless (and fps width height)
      (format t "~a Failed to retrieve fps/width/height from ~a~%" (log-tag "error") (car files))
      (format t "~a Check if the file is a valid video with proper metadata.~%" (log-tag "error"))
      (uiop:quit 1))

    (let* ((audio-enabled-p (all-have-audio-p video-infos))
           (filter (build-concat-filter files first-video-info audio-enabled-p))
           (cmd (append '("ffmpeg" "-y")
                        input-args
                        (list "-filter_complex" filter
                              "-map" "[outv]")
                        (if audio-enabled-p '("-map" "[outa]") nil)
                        (list output))))
      cmd)))

(defun build-cmd (opts output)
  "Construct the ffmpeg command list from visp-options and output filename."
  (let* ((repeat (visp-options-repeat opts))
         (input (visp-options-input opts))
         (scale (visp-options-scale opts))
         (fps (visp-options-fps opts))
         (mute (visp-options-mute opts))
         (rev (visp-options-rev opts))
         (mono (visp-options-mono opts))
         (hflip (visp-options-hflip opts))
         (vflip (visp-options-vflip opts))
         (speed (visp-options-speed opts))
         (codec-info (visp-options-codec-info opts))
         (filters '())
         (cmd (list "ffmpeg" "-y")))

    ;; Loop playback (must come before input argument)
    (when repeat
      (setf cmd (append cmd 
        (list "-stream_loop" (format nil "~a" (1- repeat))))))

    (setf cmd (append cmd (list "-i" input)))

    ;; Resolution scaling
    (when scale
      (destructuring-bind (w . h) scale
        (push (format nil "scale=~A:~A" w h) filters)))

    ;; Reverse playback
    (when rev
      (push "reverse" filters))
    
    ;; Speed adjustment
    (when speed
      (push (format nil "setpts=PTS/~a" speed) filters))
    
    ;; Video flipping
    (when hflip
      (push "hflip" filters))
    
    (when vflip
      (push "vflip" filters))
    
    (when mono
      (push "format=gray" filters))

    (when filters
      ;; Filter order matters: scale before effects like reverse
      (let* ((filters-str (format nil "~{~a~^,~}" (reverse filters)))
              (vf-arg (list "-vf" filters-str)))
        (setf cmd (append cmd vf-arg))))

    ;; Video codec settings
    (when codec-info
      (let ((encoder (getf codec-info :encoder))
            (pix-fmt (getf codec-info :pix_fmt)))
        (setf cmd (append cmd (list "-c:v" encoder)))
        (when pix-fmt
          (setf cmd (append cmd (list "-pix_fmt" pix-fmt))))))

    ;; Audio removal
    (when mute
      (setf cmd (append cmd (list "-an"))))

    ;; Frame rate
    (when fps
      (setf cmd (append cmd (list "-r" (format nil "~a" fps)))))

    ;; Add output filename
    (setf cmd (append cmd (list output))) cmd))

(defun encoder-available-p (name)
  "Return T if the given encoder name appears in `ffmpeg -encoders` output."
  (let ((output (uiop:run-program (list "ffmpeg" "-hide_banner" "-encoders")
                                  :output :string)))
    (search name output :test #'string=)))

(defun ffmpeg-available-p ()
  "Check if `ffmpeg` is available in PATH."
  (ignore-errors
    (uiop:run-program '("ffmpeg" "-version")
                      :ignore-error-status t
                      :output nil :error-output nil)
    t))