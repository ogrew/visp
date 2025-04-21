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
  (format t "  --dry-run            Print ffmpeg command without executing it~%")
  (format t "  --help               Show this help message~%~%")
  (format t "Example:~%  ./visp.ros --input movie.mp4 --res fhd --codec h264 --mute~%"))

(defun validate-options (opts)

  ;; 入力ファイルは必須
  (unless (visp-options-input opts)
    (format t "Usage: visp --input <filename> [--res 4k] [--mute] ...~%")
    (uiop:quit 1))

  (let ((res (visp-options-res opts)))
    (when res
      (let ((pair (visp:resolution-from-key res)))
        (if pair
            (setf (visp-options-scale opts) (cdr pair))
            ;; [error]対応していない解像度を指定された場合は中断
            (progn
              (format t "~a visp does not support the resolution '~a'.~%" 
                (log-tag "error") res)
              (uiop:quit 1))))))

  (let ((fps (visp-options-fps opts)))
    (when fps
      (unless (parse-integer fps :junk-allowed t)
        (format t "~a --fps must be a number, but got '~a'.~%" 
                  (log-tag "error") fps)
        (uiop:quit 1))))

  (let ((codec-info (visp:codec-info-from-key (visp-options-codec opts))))
    ;; [error]codec-infoが無効だった場合は中断
    (when (and (visp-options-codec opts) (not codec-info))
      (format t "~a visp does not support the codec '~a'.~%" 
                (log-tag "error") (visp-options-codec opts))
      (uiop:quit 1))
    ;; [error]利用環境で未サポートのコーデック
    (when (and codec-info
               (not (visp:encoder-available-p (getf codec-info :encoder))))
      (format t "~a ffmpeg on this system does not support the encoder '~a'.~%"
               (log-tag "error") (getf codec-info :encoder))
      (uiop:quit 1))
    (setf (visp-options-codec-info opts) codec-info)))