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

  (let ((input (visp-options-input opts)))
    ;; 入力ファイルは必須
    (unless input
      (format t "Usage: visp --input <filename> [--res 4k] [--mute] ...~%")
      (uiop:quit 1))

    (let ((ext (visp:input-extension input))
           (rev (visp-options-rev opts))
           (repeat (visp-options-repeat opts)))

      ;; 対応する拡張子は最低限
      (unless (member ext +allowed-input-extensions+ :test #'string-equal)
        (format t "~a visp does not support the input file extension '~a'.~%"
                (visp:log-tag "error") ext)
        (uiop:quit 1))

      (when rev
        ;; reverseはmp4かmovしかサポートしない
        (unless (member ext '(".mp4" ".mov") :test #'string-equal)
          (format t "~a --reverse option only supports .mov or .mp4 extensions.~%" (visp:log-tag "error"))
          (uiop:quit 1))
        ;; reverseはメモリ消費が大きいので警告
        (format t "~a --reverse may consume a large amount of memory. Consider using small input files.~%"
          (visp:log-tag "info")))

      ;; --reverseと--loopの併用禁止
      (when (and rev repeat)
        (format t "~a --loop and --reverse options cannot be used together.~%" (visp:log-tag "error"))
          (uiop:quit 1))
      
      ;; 逆再生は不可の観点で音声強制ミュート
      (when (and rev (not (visp-options-mute opts)))
        (format t "~a --reverse implies muted audio. Audio will be disabled.~%" (visp:log-tag "info"))
        (setf (visp-options-mute opts) t))))

  ;; repeatは1以上の整数
  (let ((repeat (visp-options-repeat opts)))
    (when repeat
      (let ((repeati (parse-integer repeat :junk-allowed t)))
        (unless (and (integerp repeati) (>= repeati 1))
          (format t "~a --loop must be an integer >= 1, but got '~a'. ~%"
                  (visp:log-tag "error") repeat)
          (uiop:quit 1))
        (setf (visp-options-repeat opts) repeati))))

  (let ((res (visp-options-res opts))
         (half (visp-options-half opts)))
    
    ;; --resと--halfは併用不可
    (when (and res half)
      (format t "~a --res and --half cannot be used together.~%" (visp:log-tag "error"))
      (uiop:quit 1))

    (when res
      (let ((pair (visp:resolution-from-key res)))
        (if pair
          (setf (visp-options-scale opts) (cdr pair))
          ;; [error]対応していない解像度を指定された場合は中断
          (progn
            (format t "~a visp does not support the resolution '~a'.~%" 
              (visp:log-tag "error") res)
            (uiop:quit 1)))))

    (when half
      (let ((dims (visp:get-video-dims (visp-options-input opts))))
        (if dims
          (destructuring-bind (w . h) dims
            (setf (visp-options-scale opts)
              (cons (floor w 2) (floor h 2))))
          (format t "~a Could not determine resolution for --half. Original size will be used.~%"
                    (visp:log-tag "warn"))))))

  (let ((fps (visp-options-fps opts)))
    (when fps
      (let ((fpsi (parse-integer fps :junk-allowed t)))
        (unless (and (integerp fpsi) (> fpsi 0))
          (format t "~a --fps must be a positive integer, but got '~a'.~%" 
                  (visp:log-tag "error") fps)
          (uiop:quit 1))
        ;; 再パースして明示的にintで上書き
        (setf (visp-options-fps opts) fpsi))))

  (let ((codec-info (visp:codec-info-from-key (visp-options-codec opts))))
    ;; [error]codec-infoが無効だった場合は中断
    (when (and (visp-options-codec opts) (not codec-info))
      (format t "~a visp does not support the codec '~a'.~%" 
                (visp:log-tag "error") (visp-options-codec opts))
      (uiop:quit 1))
    ;; [error]利用環境で未サポートのコーデック
    (when (and codec-info
               (not (visp:encoder-available-p (getf codec-info :encoder))))
      (format t "~a ffmpeg on this system does not support the encoder '~a'.~%"
               (visp:log-tag "error") (getf codec-info :encoder))
      (uiop:quit 1))
    (setf (visp-options-codec-info opts) codec-info)))