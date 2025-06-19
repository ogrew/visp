(in-package :visp)

(defun validate-gif-mode (opts)
  "Validate options for GIF mode: --gif accepts only video files and no other options."
  (let ((input (visp-options-input opts)))
    ;; 入力ファイルの存在チェック
    (unless (and input (not (string= input "")))
      (format t "~a --gif mode requires a video file. Use: visp --gif <video-file>~%"
              (log-tag "error"))
      (uiop:quit 1))


    ;; GIFモード対応拡張子チェック（全動画形式対応）
    (let ((ext (input-extension input)))
      (unless (member ext +allowed-input-extensions+ :test #'string-equal)
        (format t "~a --gif mode supports video files (.mp4, .mov, .flv, .avi, .webm), but got '~a'.~%"
                (log-tag "error") ext)
        (uiop:quit 1)))

    ;; すべての他オプションを禁止（--dry-runのみ特別に許可）
    (let ((disallowed-options (list
                                (visp-options-output opts)   ; --outputオプション禁止  
                                (visp-options-res opts)
                                (visp-options-codec opts)
                                (visp-options-scale opts)
                                (visp-options-fps opts)
                                (visp-options-repeat opts)
                                (visp-options-half opts)
                                (visp-options-rev opts)
                                (visp-options-mute opts)
                                (visp-options-mono opts)
                                (visp-options-hflip opts)
                                (visp-options-vflip opts)
                                (visp-options-speed opts)
                                (visp-options-merge-files opts))))
      (when (some #'identity disallowed-options)
        (format t "~a --gif mode does not accept any other options. Use: visp --gif <video-file> [--dry-run]~%" 
                (log-tag "error"))
        (uiop:quit 1)))))

(defun validate-merge-files (opts)
  "Validate options specific to --merge mode."
  (let* ((files (visp-options-merge-files opts)))

    ;; --dry-runと--outputだけは併用を許可する。他はすべて禁止。
    (when (or (visp-options-input opts)
              (visp-options-res opts)
              (visp-options-codec opts)
              (visp-options-fps opts)
              (visp-options-repeat opts)
              (visp-options-half opts)
              (visp-options-rev opts)
              (visp-options-mute opts)
              (visp-options-mono opts)
              (visp-options-hflip opts)
              (visp-options-vflip opts)
              (visp-options-speed opts))
      (format t "~a --merge cannot be combined with other options.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; ファイル数チェック
    (unless (and files (>= (length files) 2))
      (format t "~a At least two .mp4 files must be specified for --merge.~%" (log-tag "error"))
      (uiop:quit 1))

    ;; 拡張子チェック
    (dolist (file files)
      (unless (string-equal (input-extension file) ".mp4")
        (format t "~a Only .mp4 files are supported for --merge (got: ~a).~%" (log-tag "error") file)
        (uiop:quit 1)))

    ;; ファイル存在確認
    (dolist (file files)
      (unless (probe-file file)
        (format t "~a Input file '~a' does not exist.~%" (log-tag "error") file)
        (uiop:quit 1)))

    ;; 動画情報取得
    (let* ((infos (mapcar #'get-video-info files)))

      ;; メタ情報に不備があるファイルを検出（fps / width / height）
      (dolist (pair (mapcar #'cons files infos))
        (let ((file (car pair))
              (info (cdr pair)))
          (unless (and (getf info :fps) (getf info :width) (getf info :height))
            (format t "~a Failed to extract fps/width/height from file: ~a~%" (log-tag "error") file)
            (format t "~a This file may be corrupted or not a valid video.~%" (log-tag "error"))
            (uiop:quit 1))))

      ;; 音声混在チェック
      (let ((has-audio-list (mapcar (lambda (info) (getf info :has-audio)) infos)))
        (unless (or (every #'identity has-audio-list)
                    (every #'not has-audio-list))
          (format t "~a --merge does not support mixing audio and non-audio files.~%" (log-tag "error"))
          (uiop:quit 1)))

      ;; fps混在チェック（warn）
      (let ((fps-list (remove-duplicates (mapcar (lambda (info) (getf info :fps)) infos)
                                         :test #'=)))
        (when (> (length fps-list) 1)
          (format t "~a Detected different fps values across files. All will be converted to ~afps.~%"
                  (log-tag "warn") (getf (car infos) :fps))))

      ;; 解像度混在チェック（warn）
      (let ((size-list (remove-duplicates
                        (mapcar (lambda (info)
                                  (cons (getf info :width) (getf info :height)))
                                infos)
                        :test #'equal)))
        (when (> (length size-list) 1)
          (format t "~a Detected different resolutions across files. All will be scaled to ~ax~a.~%"
                  (log-tag "warn")
                  (getf (car infos) :width)
                  (getf (car infos) :height)))))))


(defun validate-input (opts)
  "Check if input file or directory is provided and valid. If directory, collect target files."
  (let ((input (visp-options-input opts)))
    ;; 入力の有無は必須
    (unless input
      (error-option "Input file or directory is required"
                    :option-name "input"
                    :context "Usage: visp --input <filename or directory> [--res 4k] [--mute] ..."))

    ;; ディレクトリ指定っぽい場合
    (if (uiop:directory-pathname-p input)
        (progn
          ;; ディレクトリが存在するか
          (unless (uiop:directory-exists-p input)
            (error-file "Directory does not exist"
                        :file-path input))

          ;; 対象ファイルの取得
          (let ((files (remove-if-not
                         #'(lambda (p)
                             (member (input-extension p) +allowed-input-extensions+ :test #'string-equal))
                         (uiop:directory-files input))))
            (when (null files)
              (error-file "No valid video files found in directory"
                          :file-path input))

            ;; batch-files にファイル名 (string 型) を格納
            (setf (visp-options-batch-files opts) (mapcar #'namestring files))

            (format t "~a Batch mode: ~a video file(s) found in directory.~%" (log-tag "info") (length files))))

        ;; ファイル指定の場合
        (progn
          ;; ファイル存在チェック
          (unless (probe-file input)
            (error-file "Input file does not exist"
                        :file-path input))

          ;; 拡張子チェック
          (let ((ext (input-extension input)))
            (unless (member ext +allowed-input-extensions+ :test #'string-equal)
              (error-file "Unsupported input file extension"
                          :file-path input
                          :context ext)))

            ;; 動画情報の表示
            (print-video-info (get-video-info input))))))

(defun validate-reverse (opts)
  "Validate reverse option: only allowed for .mp4/.mov, cannot be used with --loop, implies mute."
  (let* ((rev (visp-options-rev opts))
         (repeat (visp-options-repeat opts))
         (input (visp-options-input opts))
         (ext (input-extension input)))

    ;; --reverse と --loop は併用不可
    (when (and rev repeat)
      (error-option "--loop and --reverse options cannot be used together"
                    :option-name "reverse"))

    ;; --reverse がサポートする拡張子は限定
    (when rev
      (unless (member ext '(".mp4" ".mov") :test #'string-equal)
        (error-option "--reverse option only supports .mov or .mp4 extensions"
                      :option-name "reverse"
                      :context ext))

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
      (let* ((pair (resolution-from-key res))
             (dims (or (and pair (cdr pair))
                       (parse-dimensions res))))
        (if dims
            (setf (visp-options-scale opts) dims)
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


(defun validate-speed (opts)
  "Validate that --speed is a positive number if specified."
  (let ((speed (visp-options-speed opts)))
    (when speed
      (let ((speedf (parse-number speed)))
        ;; speedは必ず0より大きい数値
        (unless (and (numberp speedf) (> speedf 0))
          (error-option "--speed must be a positive number"
                        :option-name "speed"
                        :context speed))
        ;; 明示的に数値に変換して再セット
        (setf (visp-options-speed opts) speedf)))))

(defun validate-output (opts)
  "Validate the --output option: check if parent directory exists for the given path."
  (let ((output (visp-options-output opts)))
    (when output
      ;; 出力ファイルのディレクトリが存在するかチェック
      (let ((parent-dir (uiop:pathname-directory-pathname (pathname output))))
        (unless (uiop:directory-exists-p parent-dir)
          (format t "~a Output directory '~a' does not exist.~%" 
                  (log-tag "error") (namestring parent-dir))
          (uiop:quit 1))))))

(defun validate-options (opts)
  (validate-input opts)
  (validate-output opts)
  (validate-reverse opts)
  (validate-repeat opts)
  (validate-resolution opts)
  (validate-half opts)
  (validate-fps opts)
  (validate-codec opts)
  (validate-mono opts)
  (validate-speed opts))

(defun dispatch-validation (opts)
  (cond
    ((visp-options-merge-files opts)
     (validate-merge-files opts))
    ((visp-options-gif opts)
     (validate-gif-mode opts))
    (t
     (validate-options opts))))
