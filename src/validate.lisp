(in-package :visp)

(defun validate-input (opts)
  "Check if input file is provided and has a supported extension."
  (let ((input (visp-options-input opts)))
    ;; 入力ファイルは必須
    (unless input
      (format t "Usage: visp --input <filename> [--res 4k] [--mute] ...~%")
      (uiop:quit 1))

    ;; ファイルの存在確認
    (unless (probe-file input)
      (format t "~a Input file '~a' does not exist.~%" (log-tag "error") input)
      (uiop:quit 1))

    ;; 拡張子チェック
    (let ((ext (input-extension input)))
      (unless (member ext +allowed-input-extensions+ :test #'string-equal)
        (format t "~a visp does not support the input file extension '~a'.~%"
                (log-tag "error") ext)
        (uiop:quit 1)))
    
    ;; 動画情報の表示
    (print-video-info (get-video-info input))))


(defun validate-reverse (opts)
  "Validate reverse option: only allowed for .mp4/.mov, cannot be used with --loop, implies mute."
  (let* ((rev (visp-options-rev opts))
         (repeat (visp-options-repeat opts))
         (input (visp-options-input opts))
         (ext (input-extension input)))

    ;; --reverse と --loop は併用不可
    (when (and rev repeat)
      (format t "~a --loop and --reverse options cannot be used together.~%"
              (log-tag "error"))
      (uiop:quit 1))

    ;; --reverse がサポートする拡張子は限定
    (when rev
      (unless (member ext '(".mp4" ".mov") :test #'string-equal)
        (format t "~a --reverse option only supports .mov or .mp4 extensions.~%"
                (log-tag "error"))
        (uiop:quit 1))

      ;; メモリ使用に関する警告
      (format t "~a --reverse may consume a large amount of memory. Consider using small input files.~%"
              (log-tag "info"))

      ;; reverse時は強制ミュート
      (unless (visp-options-mute opts)
        (format t "~a --reverse implies muted audio. Audio will be disabled.~%"
                (log-tag "info"))
        (setf (visp-options-mute opts) t)))))

(defun validate-repeat (opts)
  "Validate the --loop (repeat) option: must be a positive integer."
  (let ((repeat (visp-options-repeat opts)))
    (when repeat
      (let ((repeati (parse-integer repeat :junk-allowed t)))
        ;; stream_loopの仕様上(repeat >= 1)がマスト
        (unless (and (integerp repeati) (>= repeati 1))
          (format t "~a --loop must be an integer >= 1, but got '~a'.~%"
                  (log-tag "error") repeat)
          (uiop:quit 1))
        ;; repeat は数値に変換して保存
        (setf (visp-options-repeat opts) repeati)))))

(defun validate-resolution (opts)
  "Validate resolution options: --res and --half must not coexist,
   and --res must be a known resolution label."
  (let ((res (visp-options-res opts))
        (half (visp-options-half opts)))

    ;; --res と --half の併用は禁止
    (when (and res half)
      (format t "~a --res and --half cannot be used together.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; --res が指定されていれば有効な解像度か確認
    (when res
      (let ((pair (resolution-from-key res)))
        (if pair
            (setf (visp-options-scale opts) (cdr pair)) ; 正常なら scale にセット
            (progn
              (format t "~a visp does not support the resolution '~a'.~%"
                      (log-tag "error") res)
              (uiop:quit 1)))))))

(defun validate-half (opts)
  "If --half is specified, get original resolution using ffprobe and set scaled dimensions."
  (when (visp-options-half opts)
    (let ((dims (get-video-dims (visp-options-input opts))))
      (if dims
          (destructuring-bind (w . h) dims
            (setf (visp-options-scale opts)
                  (cons (floor w 2) (floor h 2))))
          (format t "~a Could not determine resolution for --half. Original size will be used.~%"
                  (log-tag "warn"))))))

(defun validate-fps (opts)
  "Validate that --fps is a positive integer if specified."
  (let ((fps (visp-options-fps opts)))
    (when fps
      (let ((fpsi (parse-integer fps :junk-allowed t)))
        ;; fpsは必ず0より大きい整数
        (unless (and (integerp fpsi) (> fpsi 0))
          (format t "~a --fps must be a positive integer, but got '~a'.~%" 
                  (log-tag "error") fps)
          (uiop:quit 1))
        ;; 明示的に整数に変換して再セット
        (setf (visp-options-fps opts) fpsi)))))

(defun validate-codec (opts)
  "Validate that the given codec is supported and available in the current ffmpeg installation."
  (let* ((key (visp-options-codec opts))
         (codec-info (codec-info-from-key key)))

    ;; 指定された codec が visp の対応リストにない場合
    (when (and key (not codec-info))
      (format t "~a visp does not support the codec '~a'.~%" 
              (log-tag "error") key)
      (uiop:quit 1))

    ;; codec はあるが、システムに encoder が存在しない場合
    (when (and codec-info
               (not (encoder-available-p (getf codec-info :encoder))))
      (format t "~a ffmpeg on this system does not support the encoder '~a'.~%"
              (log-tag "error") (getf codec-info :encoder))
      (uiop:quit 1))

    ;; hapとproresはpix_fmtを指定しないといけない（そしてそれは固定値）
    (when (getf codec-info :pix_fmt)
      (let* ((fmt (getf codec-info :pix_fmt))
             (label (or (cdr (assoc fmt +pixfmt-name-map+ :test #'string=)) fmt)))
        (format t "~a Using pixel format: ~a (~a)~%" (log-tag "info") label fmt)))

    ;; 成功したら codec-info をオプションに保存
    (setf (visp-options-codec-info opts) codec-info)))

(defun validate-mono (opts)
  "Ensure --mono is not used with unsupported codecs."
  (when (visp-options-mono opts)
    (let ((codec (visp-options-codec opts)))
      (when (member codec '("prores" "hap") :test #'string=)
        (error "The --mono option is not supported with codec ~A." codec)))))


(defun validate-options (opts)
  (validate-input opts)
  (validate-reverse opts)
  (validate-repeat opts)
  (validate-resolution opts)
  (validate-half opts)
  (validate-fps opts)
  (validate-codec opts)
  (validate-mono opts))