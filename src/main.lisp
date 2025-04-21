(in-package :visp)

(defun print-help ()
  (format t "~%visp - minimal ffmpeg wrapper written in Common Lisp~%~%")
  (format t "Usage:~%  ./visp.ros --input <file> [options]~%~%")
  (format t "Required:~%  --input <file>       Input video file~%~%")
  (format t "Optional:~%")
  (format t "  --res <label>        Resolution (hd, fhd, 2k, 4k, etc)~%")
  (format t "  --fps <number>       Output framerate (e.g., 30)~%")
  (format t "  --codec <type>       Codec to use (h264, h265, prores, hap)~%")
  (format t "  --mute               Remove audio track~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Example:~%  ./visp.ros --input movie.mp4 --res fhd --codec h264 --mute~%"))

(defun validate-options (opts)

  ;; 入力ファイルは必須
  (unless (visp-options-input opts)
    (format t "Usage: visp --input <filename> [--res 4k] [--mute]~%")
    (uiop:quit 1))

  (let ((res (visp-options-res opts)))
    (when res
      (let ((scale (visp::resolution-from-key res)))
        (if scale
            (setf (visp-options-scale opts) scale)
            ;; [error]対応していない解像度を指定された場合は中断
            (progn
              (format t "Error: visp does not support the resolution '~a'.~%" res)
              (uiop:quit 1))))))

  (let ((fps (visp-options-fps opts)))
    (when fps
      (unless (parse-integer fps :junk-allowed t)
        (format t "Error: --fps must be a number, but got '~a'.~%" fps)
        (uiop:quit 1))))

  (let ((codec-info (visp::codec-info-from-key (visp-options-codec opts))))
    ;; [error]codec-infoが無効だった場合は中断
    (when (and (visp-options-codec opts) (not codec-info))
      (format t "Error: visp does not support the codec '~a'.~%" (visp-options-codec opts))
      (uiop:quit 1))
    ;; [error]利用環境で未サポートのコーデック
    (when (and codec-info
               (not (visp::encoder-available-p (getf codec-info :encoder))))
      (format t "Error: ffmpeg on this system does not support the encoder '~a'.~%"
              (getf codec-info :encoder))
      (uiop:quit 1))
    (setf (visp-options-codec-info opts) codec-info)))

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

(defun main (&optional args)
  ;; 引数がなければコマンドラインから取得
  (unless args (setf args (uiop:command-line-arguments)))
  (setf args (visp::clean-args args))

  (when (member "--help" args :test #'string=)
    (visp::print-help)
    (uiop:quit 0))

  (let ((opts (make-visp-options)))

    ;; 引数リストはオプション名から key-value か boolean かを判定して処理
    (loop for i from 0 below (length args) by 1
      do (let ((key (nth i args)))
           (cond
             ((string= key "--input")
             (when (< (1+ i) (length args))
               (setf (visp-options-input opts) (nth (1+ i) args))
               (incf i)))
             ((string= key "--res")
             (when (< (1+ i) (length args))
               (setf (visp-options-res opts) (nth (1+ i) args))
               (incf i)))
             ((string= key "--codec")
             (when (< (1+ i) (length args))
               (setf (visp-options-codec opts) (nth (1+ i) args))
               (incf i)))
             ((string= key "--fps")
             (when (< (1+ i) (length args))
               (setf (visp-options-fps opts) (nth (1+ i) args))
               (incf i)))
             ((string= key "--mute")
               (setf (visp-options-mute opts) t))
             (t
              (format t "Error: visp does not support the option '~a'.~%" key)
              (uiop:quit 1)))
      ))

    (validate-options opts)
    (let* ((ext (getf (visp-options-codec-info opts) :ext))
           (output (visp::generate-output-filename opts ext))
           (cmd (visp::build-cmd opts output)))

      (format t "[INFO] Running: ~{~a ~}~%" cmd)
      (uiop:run-program cmd :output t :error-output t))))