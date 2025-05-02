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

  (let ((opts (parse-args-to-options args)))
    (dispatch-validation opts)

    (cond
      ;; 1. GIF モード
      ((visp-options-gif opts)
       (let* ((input (visp-options-input opts))
              (fps (get-video-fps input)) 
              (output (generate-gif-output-filename input))
              (cmd (build-gif-cmd opts output fps)))
         (run-cmd cmd output (visp-options-dry-run opts))))

      ;; 2. 結合モード
      ((visp-options-merge-files opts)
       (let* ((output (generate-merge-output-filename opts))
              (cmd (build-merge-cmd opts output)))
         (run-cmd cmd output (visp-options-dry-run opts))))

      ;; 3. バッチモード
      ((visp-options-batch-files opts)
       (dolist (file (visp-options-batch-files opts))
        ;; 出力ファイル名やコマンド生成を通常モードと共通化するため、input を一時的に設定
        (setf (visp-options-input opts) file)
        ;; 出力ファイル名生成
        (let* ((ext (getf (visp-options-codec-info opts) :ext))
               (filename (generate-output-filename opts ext))         ;; ファイル名だけ
               (output (output-path-in-same-directory file filename)) ;; ディレクトリと結合してフルパス化
               (cmd (build-cmd opts output)))
          (run-cmd cmd output (visp-options-dry-run opts)))))

      ;; 4. 通常処理
      (t
       (let* ((ext (getf (visp-options-codec-info opts) :ext))
              (output (generate-output-filename opts ext))
              (cmd (build-cmd opts output)))
         (run-cmd cmd output (visp-options-dry-run opts)))))))
