(in-package :visp)

(defun main (&optional args)
  (unless (ffmpeg-available-p)
    (format t "~a ffmpeg not found in your system. Please install ffmpeg first.~%" (log-tag "error"))
    (uiop:quit 1))

  ;; Get command line arguments if not provided
  (unless args (setf args (uiop:command-line-arguments)))
  (setf args (clean-args args))

  (when (member "--help" args :test #'string=)
    (print-help)
    (uiop:quit 0))

  (let ((opts (parse-args-to-options args)))
    (dispatch-validation opts)

    (cond
      ;; GIF mode
      ((visp-options-gif opts)
       (let* ((input (visp-options-input opts))
              (fps (get-video-fps input)) 
              (output (generate-gif-output-filename opts))
              (cmd (build-gif-cmd opts output fps)))
         (run-cmd cmd output (visp-options-dry-run opts))))

      ;; Merge mode
      ((visp-options-merge-files opts)
       (let* ((output (generate-merge-output-filename opts))
              (cmd (build-merge-cmd opts output)))
         (run-cmd cmd output (visp-options-dry-run opts))))

      ;; Batch mode
      ((visp-options-batch-files opts)
       (dolist (file (visp-options-batch-files opts))
         ;; Temporarily set input for each file to reuse normal mode logic
        (setf (visp-options-input opts) file)
        ;; Generate output filename in same directory as input
        (let* ((ext (getf (visp-options-codec-info opts) :ext))
               (filename (generate-output-filename opts ext))
               (output (output-path-in-same-directory file filename))
               (cmd (build-cmd opts output)))
          (run-cmd cmd output (visp-options-dry-run opts)))))

      ;; Normal mode
      (t
       (let* ((ext (getf (visp-options-codec-info opts) :ext))
              (output (generate-output-filename opts ext))
              (cmd (build-cmd opts output)))
         (run-cmd cmd output (visp-options-dry-run opts)))))))
