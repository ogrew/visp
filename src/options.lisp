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
  batch-files
  gif       ;boolean
)

(defun parse-args-to-options (args)
  "Given raw args, return a filled visp-options struct or quit on invalid option."
  (let ((opts (make-visp-options))
        (len  (length args))
        (i    0))
    ;; Use our own index variable so we can safely advance it as we consume
    ;; option arguments.  Modifying a LOOP iteration variable is undefined
    ;; behaviour in Common Lisp.
    (loop while (< i len) do
         (let ((key (nth i args)))
           (cond
             ((string= key "--input")
              (when (< (1+ i) len)
                (setf (visp-options-input opts) (nth (1+ i) args))
                (incf i)))

             ((string= key "--res")
              (when (< (1+ i) len)
                (setf (visp-options-res opts) (nth (1+ i) args))
                (incf i)))

             ((string= key "--codec")
              (when (< (1+ i) len)
                (setf (visp-options-codec opts) (nth (1+ i) args))
                (incf i)))

             ((string= key "--fps")
              (when (< (1+ i) len)
                (setf (visp-options-fps opts) (nth (1+ i) args))
                (incf i)))

             ((string= key "--loop")
              (when (< (1+ i) len)
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
                ;; Advance to the first filename and gather until the next option
                (incf i)
                (loop while (and (< i len)
                                 (not (string-prefix-p "--" (nth i args))))
                      do (push (nth i args) files)
                         (incf i))
                (setf (visp-options-merge-files opts) (nreverse files))
                ;; step back so the outer loop processes the next option
                (decf i)))

             ((string= key "--gif")
              (when (< (1+ i) len)
                (setf (visp-options-gif opts) t)
                (setf (visp-options-input opts) (nth (1+ i) args))
                (incf i)))

             (t
              (format t "~a visp does not support the option '~a'.~%"
                      (log-tag "error") key)
              (uiop:quit 1))))
         (incf i))
    opts))
