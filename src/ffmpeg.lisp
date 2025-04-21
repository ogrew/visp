(in-package :visp)

(defun build-cmd (opts output)
  "Construct the ffmpeg command list from visp-options and output filename."
  (let ((cmd (list "ffmpeg" "-y" "-i" (visp-options-input opts))))

    ;; 解像度（縦：横）
    (when (visp-options-scale opts)
      (destructuring-bind (w . h) (visp-options-scale opts)
        (setf cmd (append cmd (list "-vf" (format nil "scale=~A:~A" w h))))))

    ;; コーデック
    (let ((codec-info (visp-options-codec-info opts)))
      (when codec-info
        (let ((encoder (getf codec-info :encoder)))
          (setf cmd (append cmd (list "-c:v" encoder))))))

    ;; ミュート
    (when (visp-options-mute opts)
      (setf cmd (append cmd (list "-an"))))

    ;; フレームレート
    (when (visp-options-fps opts)
      (setf cmd (append cmd (list "-r" (visp-options-fps opts)))))

    ;; 出力ファイル名
    (setf cmd (append cmd (list output)))
    cmd))

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
