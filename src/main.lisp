(in-package :visp)

(defun main (&optional args)
  ;; 引数がなければコマンドラインから取得
  (unless args (setf args (uiop:command-line-arguments)))
  (setf args (visp::clean-args args))

  (when (member "--help" args :test #'string=)
    (visp::print-help)
    (uiop:quit 0))

  (let ((opts (visp::parse-args-to-options args)))  ;;引数解析
    (visp::validate-options opts)                   ;;バリデーション

    (let* ((ext (getf (visp-options-codec-info opts) :ext))
           (output (visp::generate-output-filename opts ext)) ;;出力ファイル名作成
           (cmd (visp::build-cmd opts output)))               ;;コマンド構築

      (format t "[INFO] Running: ~{~a ~}~%" cmd)              ;;実行ログ
      (uiop:run-program cmd :output t :error-output t))))