(in-package :visp)

(defun main (&optional args)
  (unless args
    (setf args (uiop:command-line-arguments)))
  (let ((input (loop for (key val) on args by #'cddr
                     when (string= key "--input")
                     return val)))
    (if input
        (let ((cmd (list "ffmpeg" "-y" "-i" input "output.mp4")))
          (uiop:run-program cmd :output t :error-output t))
        (format t "Usage: visp --input <filename>~%"))))
