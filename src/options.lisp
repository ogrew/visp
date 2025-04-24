(in-package :visp)

(defstruct visp-options
  input
  res
  codec
  codec-info
  scale
  fps
  repeat
  half      ;boolean
  rev       ;boolean
  mute      ;boolean
  mono      ;boolean
  dry-run   ;boolean
  merge-files
)

(defun parse-args-to-options (args)
  "Given raw args, return a filled visp-options struct or quit on invalid option."
  (let ((opts (make-visp-options)))
    (loop for i from 0 below (length args) by 1
          do (let ((key (nth i args)))
               (cond
                 ((string= key "--input")
                  (when (< (1+ i) (length args))
                    (setf (visp-options-input opts) (nth (1+ i) args))
                    (incf i)))
                 ((string= key "--res")
                  (when (< (1+ i) (length args))
                    (setf (visp-options-res opts) (nth (1+ i) args))
                    (incf i)))
                 ((string= key "--codec")
                  (when (< (1+ i) (length args))
                    (setf (visp-options-codec opts) (nth (1+ i) args))
                    (incf i)))
                 ((string= key "--fps")
                  (when (< (1+ i) (length args))
                    (setf (visp-options-fps opts) (nth (1+ i) args))
                    (incf i)))
                 ((string= key "--loop")
                  (when (< (1+ i) (length args))
                    (setf (visp-options-repeat opts) (nth (1+ i) args))
                    (incf i)))
                 ((string= key "--mute")
                  (setf (visp-options-mute opts) t))
                 ((string= key "--half")
                  (setf (visp-options-half opts) t))
                 ((string= key "--reverse")
                  (setf (visp-options-rev opts) t))
                 ((string= key "--mono")
                  (setf (visp-options-mono opts) t))
                 ((string= key "--dry-run")
                  (setf (visp-options-dry-run opts) t))
                 ((string= key "--merge")
                  (let ((files '()))
                    (loop for j from (1+ i) below (length args)
                          for val = (nth j args)
                          until (string-prefix-p "--" val)
                          do (push val files)
                              (incf i))
                    (setf (visp-options-merge-files opts) (nreverse files))))
                  (t
                  (format t "~a visp does not support the option '~a'.~%" (log-tag "error") key)
                  (uiop:quit 1)))))
    opts))
