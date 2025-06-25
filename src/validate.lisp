(in-package :visp)

(defun validate-gif-mode (opts)
  "Validate options for GIF mode: --gif accepts only video files and no other options."
  (let ((input (visp-options-input opts)))
    ;; Check if input file exists
    (unless (and input (not (string= input "")))
      (format t "~a --gif mode requires a video file. Use: visp --gif <video-file>~%"
              (log-tag "error"))
      (uiop:quit 1))


    ;; Check supported video file extensions
    (let ((ext (input-extension input)))
      (unless (member ext +allowed-input-extensions+ :test #'string-equal)
        (format t "~a --gif mode supports video files (.mp4, .mov, .flv, .avi, .webm), but got '~a'.~%"
                (log-tag "error") ext)
        (uiop:quit 1)))

    ;; Disallow all other options except --dry-run
    (let ((disallowed-options (list
                                (visp-options-output opts)
                                (visp-options-res opts)
                                (visp-options-codec opts)
                                (visp-options-scale opts)
                                (visp-options-fps opts)
                                (visp-options-repeat opts)
                                (visp-options-half opts)
                                (visp-options-rev opts)
                                (visp-options-mute opts)
                                (visp-options-mono opts)
                                (visp-options-hflip opts)
                                (visp-options-vflip opts)
                                (visp-options-speed opts)
                                (visp-options-merge-files opts))))
      (when (some #'identity disallowed-options)
        (format t "~a --gif mode does not accept any other options. Use: visp --gif <video-file> [--dry-run]~%" 
                (log-tag "error"))
        (uiop:quit 1)))))

(defun validate-merge-files (opts)
  "Validate options specific to --merge mode."
  (let* ((files (visp-options-merge-files opts)))

    ;; Only allow --dry-run and --output with --merge
    (when (or (visp-options-input opts)
              (visp-options-res opts)
              (visp-options-codec opts)
              (visp-options-fps opts)
              (visp-options-repeat opts)
              (visp-options-half opts)
              (visp-options-rev opts)
              (visp-options-mute opts)
              (visp-options-mono opts)
              (visp-options-hflip opts)
              (visp-options-vflip opts)
              (visp-options-speed opts))
      (format t "~a --merge cannot be combined with other options.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; Check minimum file count
    (unless (and files (>= (length files) 2))
      (format t "~a At least two .mp4 files must be specified for --merge.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; Check file extensions (merge mode requires .mp4)
    (dolist (file files)
      (unless (string-equal (input-extension file) ".mp4")
        (format t "~a Only .mp4 files are supported for --merge (got: ~a).~%" (log-tag "error") file)
        (uiop:quit 1)))

    ;; Check file existence
    (dolist (file files)
      (unless (probe-file file)
        (format t "~a Input file '~a' does not exist.~%" (log-tag "error") file)
        (uiop:quit 1)))

    ;; Get video metadata
    (let* ((infos (mapcar #'get-video-info files)))

      ;; Detect files with incomplete metadata (fps/width/height)
      (dolist (pair (mapcar #'cons files infos))
        (let ((file (car pair))
              (info (cdr pair)))
          (unless (and (getf info :fps) (getf info :width) (getf info :height))
            (format t "~a Failed to extract fps/width/height from file: ~a~%" (log-tag "error") file)
            (format t "~a This file may be corrupted or not a valid video.~%" (log-tag "error"))
            (uiop:quit 1))))

      ;; Check for mixed audio/no-audio files
      (let ((has-audio-list (mapcar (lambda (info) (getf info :has-audio)) infos)))
        (unless (or (every #'identity has-audio-list)
                    (every #'not has-audio-list))
          (format t "~a --merge does not support mixing audio and non-audio files.~%" (log-tag "error"))
          (uiop:quit 1)))

      ;; Warn about mixed frame rates
      (let ((fps-list (remove-duplicates (mapcar (lambda (info) (getf info :fps)) infos)
                                         :test #'=)))
        (when (> (length fps-list) 1)
          (format t "~a Detected different fps values across files. All will be converted to ~afps.~%"
                  (log-tag "warn") (getf (car infos) :fps))))

      ;; Warn about mixed resolutions
      (let ((size-list (remove-duplicates
                        (mapcar (lambda (info)
                                  (cons (getf info :width) (getf info :height)))
                                infos)
                        :test #'equal)))
        (when (> (length size-list) 1)
          (format t "~a Detected different resolutions across files. All will be scaled to ~ax~a.~%"
                  (log-tag "warn")
                  (getf (car infos) :width)
                  (getf (car infos) :height)))))))


(defun validate-input (opts)
  "Check if input file or directory is provided and valid. If directory, collect target files."
  (let ((input (visp-options-input opts)))
    ;; Input is required
    (unless input
      (format t "Usage: visp --input <filename or directory> [--res 4k] [--mute] ...~%")
      (uiop:quit 1))

    ;; Handle directory input
    (if (uiop:directory-pathname-p input)
        (progn
          ;; Check if directory exists
          (unless (uiop:directory-exists-p input)
            (format t "~a Directory '~a' does not exist.~%" (log-tag "error") input)
            (uiop:quit 1))

          ;; Get valid video files from directory
          (let ((files (remove-if-not
                         #'(lambda (p)
                             (member (input-extension p) +allowed-input-extensions+ :test #'string-equal))
                         (uiop:directory-files input))))
            (when (null files)
              (format t "~a No valid video files found in directory '~a'.~%" (log-tag "error") input)
              (uiop:quit 1))

            ;; Store file paths for batch processing
            (setf (visp-options-batch-files opts) (mapcar #'namestring files))

            (format t "~a Batch mode: ~a video file(s) found in directory.~%" (log-tag "info") (length files))))

        ;; Handle single file input
        (progn
          ;; Check if file exists
          (unless (probe-file input)
            (format t "~a Input file '~a' does not exist.~%" (log-tag "error") input)
            (uiop:quit 1))

          ;; Check file extension
          (let ((ext (input-extension input)))
            (unless (member ext +allowed-input-extensions+ :test #'string-equal)
              (format t "~a visp does not support the input file extension '~a'.~%"
                      (log-tag "error") ext)
              (uiop:quit 1)))

            ;; Display video information
            (print-video-info (get-video-info input))))))

(defun validate-reverse (opts)
  "Validate reverse option: only allowed for .mp4/.mov, cannot be used with --loop, implies mute."
  (let* ((rev (visp-options-rev opts))
         (repeat (visp-options-repeat opts))
         (input (visp-options-input opts))
         (ext (input-extension input)))

    ;; --reverse and --loop cannot be used together
    (when (and rev repeat)
      (format t "~a --loop and --reverse options cannot be used together.~%"
              (log-tag "error"))
      (uiop:quit 1))

    ;; --reverse only supports specific file extensions
    (when rev
      (unless (member ext '(".mp4" ".mov") :test #'string-equal)
        (format t "~a --reverse option only supports .mov or .mp4 extensions.~%"
                (log-tag "error"))
        (uiop:quit 1))

      ;; Memory usage warning
      (format t "~a --reverse may consume a large amount of memory. Consider using small input files.~%"
              (log-tag "info"))

      ;; Force mute when reversing
      (unless (visp-options-mute opts)
        (format t "~a --reverse implies muted audio. Audio will be disabled.~%"
                (log-tag "info"))
        (setf (visp-options-mute opts) t)))))

(defun validate-repeat (opts)
  "Validate the --loop (repeat) option: must be a positive integer."
  (let ((repeat (visp-options-repeat opts)))
    (when repeat
      (let ((repeati (parse-integer repeat :junk-allowed t)))
        ;; stream_loop requires repeat >= 1
        (unless (and (integerp repeati) (>= repeati 1))
          (format t "~a --loop must be an integer >= 1, but got '~a'.~%"
                  (log-tag "error") repeat)
          (uiop:quit 1))
        ;; Convert and store as integer
        (setf (visp-options-repeat opts) repeati)))))

(defun validate-resolution (opts)
  "Validate resolution options: --res and --half must not coexist,
   and --res must be a known resolution label."
  (let ((res (visp-options-res opts))
        (half (visp-options-half opts)))

    ;; --res and --half cannot be used together
    (when (and res half)
      (format t "~a --res and --half cannot be used together.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; Validate resolution if specified
    (when res
      (let* ((pair (resolution-from-key res))
             (dims (or (and pair (cdr pair))
                       (parse-dimensions res))))
        (if dims
            (setf (visp-options-scale opts) dims)
            (progn
              (format t "~a visp does not support the resolution '~a'.~%"
                      (log-tag "error") res)
              (uiop:quit 1)))))))

(defun validate-half (opts)
  "If --half is specified, get original resolution using ffprobe and set scaled dimensions."
  (when (visp-options-half opts)
    (let ((dims (get-video-dims (visp-options-input opts))))
      (if dims
          (destructuring-bind (w . h) dims
            (setf (visp-options-scale opts)
                  (cons (floor w 2) (floor h 2))))
          (format t "~a Could not determine resolution for --half. Original size will be used.~%"
                  (log-tag "warn"))))))

(defun validate-fps (opts)
  "Validate that --fps is a positive integer if specified."
  (let ((fps (visp-options-fps opts)))
    (when fps
      (let ((fpsi (parse-integer fps :junk-allowed t)))
        ;; FPS must be positive integer
        (unless (and (integerp fpsi) (> fpsi 0))
          (format t "~a --fps must be a positive integer, but got '~a'.~%" 
                  (log-tag "error") fps)
          (uiop:quit 1))
        ;; Convert to integer
        (setf (visp-options-fps opts) fpsi)))))

(defun validate-codec (opts)
  "Validate that the given codec is supported and available in the current ffmpeg installation."
  (let* ((key (visp-options-codec opts))
         (codec-info (codec-info-from-key key)))

    ;; Check if codec is supported by visp
    (when (and key (not codec-info))
      (format t "~a visp does not support the codec '~a'.~%" 
              (log-tag "error") key)
      (uiop:quit 1))

    ;; Check if encoder is available in system ffmpeg
    (when (and codec-info
               (not (encoder-available-p (getf codec-info :encoder))))
      (format t "~a ffmpeg on this system does not support the encoder '~a'.~%"
              (log-tag "error") (getf codec-info :encoder))
      (uiop:quit 1))

    ;; Some codecs require specific pixel formats
    (when (getf codec-info :pix_fmt)
      (let* ((fmt (getf codec-info :pix_fmt))
             (label (or (cdr (assoc fmt +pixfmt-name-map+ :test #'string=)) fmt)))
        (format t "~a Using pixel format: ~a (~a)~%" (log-tag "info") label fmt)))

    ;; Store codec info if validation succeeds
    (setf (visp-options-codec-info opts) codec-info)))

(defun validate-mono (opts)
  "Ensure --mono is not used with unsupported codecs."
  (when (visp-options-mono opts)
    (let ((codec (visp-options-codec opts)))
      (when (member codec '("prores" "hap") :test #'string=)
        (error "The --mono option is not supported with codec ~A." codec)))))


(defun validate-speed (opts)
  "Validate that --speed is a positive number if specified."
  (let ((speed (visp-options-speed opts)))
    (when speed
      (let ((speedf (parse-number speed)))
        ;; Speed must be positive number
        (unless (and (numberp speedf) (> speedf 0))
          (format t "~a --speed must be a positive number, but got '~a'.~%" 
                  (log-tag "error") speed)
          (uiop:quit 1))
        ;; Convert to number
        (setf (visp-options-speed opts) speedf)))))

(defun validate-output (opts)
  "Validate the --output option: check if parent directory exists for the given path."
  (let ((output (visp-options-output opts)))
    (when output
      ;; Check if output directory exists
      (let ((parent-dir (uiop:pathname-directory-pathname (pathname output))))
        (unless (uiop:directory-exists-p parent-dir)
          (format t "~a Output directory '~a' does not exist.~%" 
                  (log-tag "error") (namestring parent-dir))
          (uiop:quit 1))))))

(defun validate-options (opts)
  (validate-input opts)
  (validate-output opts)
  (validate-reverse opts)
  (validate-repeat opts)
  (validate-resolution opts)
  (validate-half opts)
  (validate-fps opts)
  (validate-codec opts)
  (validate-mono opts)
  (validate-speed opts))

(defun dispatch-validation (opts)
  (cond
    ((visp-options-merge-files opts)
     (validate-merge-files opts))
    ((visp-options-gif opts)
     (validate-gif-mode opts))
    (t
     (validate-options opts))))
