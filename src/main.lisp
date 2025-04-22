(in-package :visp)

(defun main (&optional args)

  (unless (ffmpeg-available-p)
    (format t "~a ffmpeg not found in your system. Please install ffmpeg first.~%" (log-tag "error"))
    (uiop:quit 1))

  ;; 引数がなければコマンドラインから取得
  (unless args (setf args (uiop:command-line-arguments)))
  (setf args (visp:clean-args args))

  (when (member "--help" args :test #'string=)
    (visp:print-help)
    (uiop:quit 0))

  (let ((opts (visp:parse-args-to-options args)))  ;;引数解析
    (visp:validate-options opts)                   ;;バリデーション

    (let* ((ext (getf (visp-options-codec-info opts) :ext))
           (output (visp:generate-output-filename opts ext)) ;;出力ファイル名作成
           (cmd (visp:build-cmd opts output)))               ;;コマンド構築

    (if (visp-options-dry-run opts)
        (progn
          (format t "~a Planned output file: ~a~%" (log-tag "info") output)
          (format t "~a Command: ~{~a ~}~%" (log-tag "dry-run") cmd))
        (progn
          (format t "~a Running: ~{~a ~}~%" (log-tag "info") cmd)
          (uiop:run-program cmd :output t :error-output t)))
    )))