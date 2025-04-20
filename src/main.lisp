(in-package :visp)

(defun build-cmd (input scale codec-info mute output)
  "Constructs the ffmpeg command line list."
  (let ((cmd (list "ffmpeg" "-y" "-i" input)))

    ;; 解像度オプション
    (when scale
      (destructuring-bind (w . h) scale
        (setf cmd (append cmd (list "-vf" (format nil "scale=~A:~A" w h))))))

    ;; コーデックオプション
    (when codec-info
      (let ((encoder (getf codec-info :encoder)))
        (setf cmd (append cmd (list "-c:v" encoder)))))

    ;; muteオプション
    (when mute
      (setf cmd (append cmd (list "-an"))))

    ;; 出力ファイル名
    (setf cmd (append cmd (list output)))
    cmd))

(defun main (&optional args)
  ;; 引数がなければコマンドラインから取得
  (unless args
    (setf args (uiop:command-line-arguments)))

  (setf args (visp::clean-args args))  ; 全角スペースを正規化

  ;; オプション用の変数
  (let ((input nil)
        (res nil)
        (codec nil)
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
             ((string= key "--codec")
             (when (< (1+ i) (length args))
               (setf codec (nth (1+ i) args))
               (incf i)))
             ((string= key "--mute")
              (setf mute t))
             (t
              (format t "Error: visp does not support the option '~a'.~%" key)
              (uiop:quit 1)))
      ))

    ;; 必須チェック
    (unless input
      (format t "Usage: visp --input <filename> [--res 4k] [--mute]~%")
      (return-from main))

    (let* ((scale (and res (visp::resolution-from-key res)))
           (codec-info (visp::codec-info-from-key codec)))

    ;; 引数のコーデックが無効だった場合,終了
    (when (and codec (not codec-info))
      (format t "Error: visp does not support the codec '~a'.~%" codec)
      (uiop:quit 1))

    ;; システムでサポートしていないエンコーダだった場合
    (when (and codec-info
              (not (visp::encoder-available-p (getf codec-info :encoder))))
      (format t "Error: ffmpeg on this system does not support the encoder '~a'.~%"
              (getf codec-info :encoder))
      (uiop:quit 1))

    (let* ((ext (getf codec-info :ext))
       (output (visp::generate-output-filename input res mute ext))
       (cmd (visp::build-cmd input (cdr scale) codec-info mute output))) ; ← scaleはcdrで渡す

      (format t "[INFO] Running: ~{~a ~}~%" cmd)
      (uiop:run-program cmd :output t :error-output t)))))