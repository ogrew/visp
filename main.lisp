(in-package :visp)

(defun main (&optional args)
  ;; 引数がなければコマンドラインから取得
  (unless args
    (setf args (uiop:command-line-arguments)))

  ;; オプション用の変数
  (let ((input nil)
        (res nil)
        (mute nil))

    ;; 1つずつ処理しながら key-value や boolean を判定
    (loop for i from 0 below (length args) by 1
      do (let ((key (nth i args)))
           (cond
             ((string= key "--input")
             (when (< (1+ i) (length args))
               (setf input (nth (1+ i) args))
               (incf i)))
             ((string= key "--res")
             (when (< (1+ i) (length args))
               (setf res (nth (1+ i) args))
               (incf i)))
             ((string= key "--mute")
              (setf mute t))
             ;; 無視：不明なオプションは今はスルー。将来警告を追加しても良い。
             )))

    ;; 必須チェック
    (unless input
      (format t "Usage: visp --input <filename> [--res 4k] [--mute]~%")
      (return-from main))

    ;; 解像度変換（指定されていれば）
    (let* ((scale (and res (visp::resolution-from-key res)))
           (cmd (list "ffmpeg" "-y" "-i" input)))

      ;; scale オプション
      (when scale
        (destructuring-bind (w . h) (cdr scale)
          (setf cmd (append cmd (list "-vf" (format nil "scale=~A:~A" w h))))))

      ;; mute オプション
      (when mute
        (setf cmd (append cmd (list "-an"))))

      ;; 出力ファイル名（仮）
      (setf cmd (append cmd (list "output.mp4")))

      ;; 実行
      (format t "[INFO] Running: ~{~a ~}~%" cmd)
      (uiop:run-program cmd :output t :error-output t))))