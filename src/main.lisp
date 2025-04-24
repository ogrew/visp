(in-package :visp)

(defun main (&optional args)

  (unless (ffmpeg-available-p)
    (format t "~a ffmpeg not found in your system. Please install ffmpeg first.~%" (log-tag "error"))
    (uiop:quit 1))

  ;; 引数がなければコマンドラインから取得
  (unless args (setf args (uiop:command-line-arguments)))
  (setf args (clean-args args))

  (when (member "--help" args :test #'string=)
    (print-help)
    (uiop:quit 0))

  (let ((opts (parse-args-to-options args)))  ;;引数解析
        (dispatch-validation opts)            ;;バリデーション

    (if (visp-options-merge-files opts)

      ;; 動画結合ルート
      (let* ((output (generate-merge-output-filename opts))
            (cmd (build-merge-cmd opts output)))
          (run-cmd cmd output (visp-options-dry-run opts)))
      
      ;;通常ルート
      (let* ((ext (getf (visp-options-codec-info opts) :ext))
            (output (generate-output-filename opts ext))
            (cmd (build-cmd opts output)))
          (run-cmd cmd output (visp-options-dry-run opts))))))