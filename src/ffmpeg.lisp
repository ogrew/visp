(in-package :visp)

(defun build-cmd (opts output)
  "Construct the ffmpeg command list from visp-options and output filename."
  (let* ((repeat (visp-options-repeat opts))
         (input (visp-options-input opts))
         (scale (visp-options-scale opts))
         (fps (visp-options-fps opts))
         (mute (visp-options-mute opts))
         (rev (visp-options-rev opts))
         (codec-info (visp-options-codec-info opts))
         (filters '())
         (cmd (list "ffmpeg" "-y")))

    ;; ループ再生（inputより先に無いといけないらしい）
    (when repeat
      (setf cmd (append cmd 
        (list "-stream_loop" (format nil "~a" (1- repeat))))))

    (setf cmd (append cmd (list "-i" input)))

    ;; 解像度（縦：横）
    (when scale
      (destructuring-bind (w . h) scale
        (push (format nil "scale=~A:~A" w h) filters)))

    ;; 逆再生
    (when rev
      (push "reverse" filters))

    (when filters
      ;; OK: -vf "scale=1920:1080,reverse"
      ;; NG: -vf "reverse,scale=1920:1080"
      (let* ((filters-str (format nil "~{~a~^,~}" (reverse filters)))
              (vf-arg (list "-vf" filters-str)))
        (setf cmd (append cmd vf-arg))))

    ;; コーデック
    (when codec-info
      (let ((encoder (getf codec-info :encoder))
            (pix-fmt (getf codec-info :pix_fmt)))
        (setf cmd (append cmd (list "-c:v" encoder)))
        (when pix-fmt
          (setf cmd (append cmd (list "-pix_fmt" pix-fmt))))))

    ;; ミュート
    (when mute
      (setf cmd (append cmd (list "-an"))))

    ;; フレームレート
    (when fps
      (setf cmd (append cmd (list "-r" (format nil "~a" fps))))

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