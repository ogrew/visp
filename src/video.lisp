(in-package :visp)

(defun get-video-dims (input)
  "Return a cons (width . height) of the video using ffprobe."
  (let* ((cmd (list "ffprobe" "-v" "error" 
                    "-select_streams" "v:0"
                    "-show_entries" "stream=width,height"
                    "-of" "csv=p=0"
                    input))
         (output (string-trim '(#\Newline) (uiop:run-program cmd :output :string)))
         (parts (uiop:split-string output :separator ",")))
    (when (= (length parts) 2)
      (let ((w (parse-integer (first parts)))
            (h (parse-integer (second parts))))
        (cons w h)))))

(defun get-video-fps (input)
  "Extract the frame rate (fps) of the video as a float using ffprobe."
  (let* ((cmd (list "ffprobe" "-v" "error"
                    "-select_streams" "v:0"
                    "-show_entries" "stream=avg_frame_rate"
                    "-of" "default=noprint_wrappers=1:nokey=1"
                    input))
         (output (string-trim '(#\Newline) (uiop:run-program cmd :output :string)))
         (parts (uiop:split-string output :separator "/")))
    (cond
      ((= (length parts) 2)
       (/ (parse-number-or-exit (first parts) "fps") (parse-number-or-exit (second parts) "fps")))
      ((= (length parts) 1)
       (parse-number-or-exit (first parts) "fps"))
      (t
       (format t "~a Failed to extract FPS from input: ~a~%" (log-tag "error") output)
       (uiop:quit 1)))))

(defun get-video-info (input)
  "Return an alist of video info: resolution, fps, duration, codec, audio codec, etc."
  (let* ((cmd (list "ffprobe" "-v" "error"
                    "-select_streams" "v"
                    "-show_entries" "stream=width,height,r_frame_rate,codec_name"
                    "-show_entries" "format=duration"
                    "-of" "default=noprint_wrappers=1:nokey=0"
                    input))
         (raw (uiop:run-program cmd :output :string))
         (lines (uiop:split-string raw :separator '(#\Newline)))
         (info (loop for line in lines
                     for parts = (uiop:split-string line :separator "=")
                     when (= (length parts) 2)
                     collect (cons (first parts) (second parts))))
         (codec (assoc "codec_name" info :test #'string=))
         (width (assoc "width" info :test #'string=))
         (height (assoc "height" info :test #'string=))
         (rate (assoc "r_frame_rate" info :test #'string=))
         (duration (assoc "duration" info :test #'string=)))

    ;; Get audio information
    (let* ((audio-cmd (list "ffprobe" "-v" "error"
                            "-select_streams" "a"
                            "-show_entries" "stream=codec_name"
                            "-of" "default=noprint_wrappers=1:nokey=1"
                            input))
           (audio-codec (string-trim '(#\Newline)
                                     (uiop:run-program audio-cmd :output :string)))
           (has-audio (not (string= audio-codec "")))) ; Empty string means no audio
      
      `(:width ,(when width (parse-integer (cdr width)))
        :height ,(when height (parse-integer (cdr height)))
        :fps ,(when rate (parse-frame-rate (cdr rate)))
        :duration ,(when duration (parse-number (cdr duration)))
        :has-audio ,has-audio
        :audio-codec ,(if has-audio audio-codec nil)
        :video-codec ,(when codec (cdr codec))))))

(defun print-video-info (info)
  (let* ((vcodec   (getf info :video-codec))
         (w        (getf info :width))
         (h        (getf info :height))
         (fps      (getf info :fps))
         (duration (getf info :duration))
         (acodec   (getf info :audio-codec))
         (duration-str (if duration (format nil "~,2f sec" duration) "N/A")))

    (format t "~%~a Input File Info~%" (visp:log-tag "info"))
    (format t "  Video Codec: ~a~%" (or vcodec "N/A"))
    (format t "  Resolution : ~a~%" (if (and w h) (format nil "~ax~a" w h) "N/A"))
    (format t "  FPS        : ~a~%" (or fps "N/A"))
    (format t "  Duration   : ~a~%" duration-str)
    (format t "  Audio      : ~a~%~%" (or acodec "none"))))


(defun all-have-audio-p (video-infos)
  (every (lambda (info) (getf info :has-audio)) video-infos))