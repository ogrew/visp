(in-package :visp)

(defun parse-float (str)
  "Parse a string as a float. Returns NIL if not parsable."
  (handler-case
      (let ((val (read-from-string str)))
        (if (floatp val)
            val
            (coerce val 'float))) ; int → float に変換
    (error () nil)))

(defun parse-frame-rate (rate)
  (handler-case
      (let ((parts (uiop:split-string rate :separator "/")))
        (if (= (length parts) 2)
            (/ (parse-float (first parts)) (parse-float (second parts)))
            (parse-float rate)))
    (error () nil)))

(defun input-extension (filename)
  "Return the extension of the input file in lowercase, with leading dot (e.g., .mp4)"
  (let* ((base (file-namestring filename))
         (dot-pos (position #\. base :from-end t)))
    (if dot-pos
        (concatenate 'string "." (string-downcase (subseq base (1+ dot-pos))))
        "")))

(defun resolution-from-key (key)
  "Return (width . height) if key is valid; otherwise NIL"
  (assoc key +resolution-map+ :test #'string-equal))

(defun codec-info-from-key (key)
  "Return plist (:encoder \"libx264\" :ext \"mp4\") if key is valid; otherwise NIL."
  (cdr (assoc key +codec-map+ :test #'string-equal)))

(defun generate-merge-output-filename (opts)
  "Generate output filename for --merge mode, based on the first file in the list."
  (let* ((files (visp-options-merge-files opts))
         (head (car files))
         (base (file-namestring head))
         (dot-pos (position #\. base :from-end t))
         (name (subseq base 0 dot-pos)))
      (concatenate 'string name "_merged.mp4")))

(defun generate-output-filename (opts &optional ext)
  "Generate output filename based on visp-options and optional ext override."
  (let* ((input (visp-options-input opts))
         (base (file-namestring input))
         (dot-pos (position #\. base :from-end t))
         (name (subseq base 0 dot-pos))
         ;; 拡張子は codec-info の ext 優先、なければ元ファイルの拡張子
         (ext (or ext (subseq base dot-pos)))
         (res (visp-options-res opts))
         (res-suffix (if res (format nil "_~a" res) ""))
         (fps (visp-options-fps opts))
         (fps-suffix (if fps (format nil "_~afps" fps) ""))
         (repeat (visp-options-repeat opts))
         (repeat-suffix (if repeat (format nil "_x~a" repeat) ""))
         (half (visp-options-half opts))
         (half-suffix (if half "_Half" ""))
         (mono (visp-options-mono opts))
         (mono-suffix (if mono "_Gray" ""))
         (rev (visp-options-rev opts))
         (rev-suffix (if rev "_Reverse" ""))
         (mute (visp-options-mute opts))
         (mute-suffix (if mute "_noSound" "")))
    (apply #'concatenate 'string 
      (list name 
            res-suffix 
            fps-suffix 
            mute-suffix 
            rev-suffix 
            half-suffix 
            mono-suffix 
            repeat-suffix 
            ext))))

(defun string-replace (str from to)
  "Replace all instances of character FROM with TO in STR."
  (coerce
   (map 'list (lambda (c) (if (char= c from) to c)) str)
   'string))

(defun clean-args (args)
  "Normalize args: replace full-width spaces and downcase all."
  (mapcar (lambda (s)
            (string-downcase
             (string-replace s #\u3000 #\Space)))
          args))

(defun string-prefix-p (prefix str)
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))