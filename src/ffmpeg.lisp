(in-package :visp)

(defun build-cmd (opts output)
  "Construct the ffmpeg command list from visp-options and output filename."
  (let* ((loop (visp-options-loop opts))
         (input (visp-options-input opts))
         (scale (visp-options-scale opts))
         (fps (visp-options-fps opts))
         (mute (visp-options-mute opts))
         (codec-info (visp-options-codec-info opts))
         (cmd (list "ffmpeg" "-y")))

    ;; ループ再生（inputより先に無いといけないらしい）
    (when loop
      (setf cmd (append cmd 
        (list "-stream_loop" (format nil "~a" (1- loop))))))

    (setf cmd (append cmd (list "-i" input)))

    ;; 解像度（縦：横）
    (when scale
      (destructuring-bind (w . h) scale
        (setf cmd (append cmd (list "-vf" (format nil "scale=~A:~A" w h))))))

    ;; コーデック
    (when codec-info
      (let ((encoder (getf codec-info :encoder)))
        (setf cmd (append cmd (list "-c:v" encoder)))))

    ;; ミュート
    (when mute
      (setf cmd (append cmd (list "-an"))))

    ;; フレームレート
    (when fps
      (setf cmd (append cmd (list "-r" fps))))

    ;; 出力ファイル名
    (setf cmd (append cmd (list output))) cmd))

(defun encoder-available-p (name)
  "Return T if the given encoder name appears in `ffmpeg -encoders` output."
  (let ((output (uiop:run-program (list "ffmpeg" "-hide_banner" "-encoders")
                                  :output :string)))
    (search name output :test #'string=)))

(defun ffmpeg-available-p ()
  "Check if `ffmpeg` is available in PATH."
  (ignore-errors
    (uiop:run-program '("ffmpeg" "-version")
                      :ignore-error-status t
                      :output nil :error-output nil)
    t))
