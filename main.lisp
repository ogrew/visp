(in-package :visp)

(defun main (&optional args)
  ;; If no args provided, try to use command-line arguments
  (unless args
    (setf args (uiop:command-line-arguments)))

  ;; Debug: Print the arguments we received
  (format t "Debug: Arguments in main function: ~S~%" args)
  
  (let ((input (loop for (key val) on args by #'cddr
                     when (string= key "--input")
                     return val)))
    (if input
        (let ((cmd (list "ffmpeg" "-y" "-i" input "output.mp4")))
          (format t "Would run: ffmpeg -y -i ~a output.mp4~%" input)
          (format t "Running ffmpeg...~%")
          (uiop:run-program cmd :output t :error-output t)
          (format t "Conversion complete. Output saved to output.mp4~%"))
        (format t "Usage: visp --input <filename>~%"))))

