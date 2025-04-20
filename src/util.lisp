(in-package :visp)

(defconstant +resolution-map+
  '(("hd"     . (1280 . 720))
    ("720p"   . (1280 . 720))
    ("fhd"    . (1920 . 1080))
    ("1080p"  . (1920 . 1080))
    ("2k"     . (2560 . 1440))
    ("4k"     . (3840 . 2160))
    ("8k"     . (7680 . 4320))))

(defparameter +codec-map+
  '(("h264"  . (:encoder "libx264"  :ext ".mp4"))
    ("h265"  . (:encoder "libx265"  :ext ".mp4"))
    ("hevc"  . (:encoder "libx265"  :ext ".mp4")) ; alias
    ("prores" . (:encoder "prores_ks" :ext ".mov"))
    ("hap"    . (:encoder "hap"     :ext ".mov"))))

(defun resolution-from-key (key)
  "Return (width . height) if key is valid; otherwise NIL"
  (assoc key +resolution-map+ :test #'string-equal))

(defun codec-info-from-key (key)
  "Return plist (:encoder \"libx264\" :ext \"mp4\") if key is valid; otherwise NIL."
  (cdr (assoc key +codec-map+ :test #'string-equal)))

(defun generate-output-filename (input res mute &optional ext)
  "Return a filename like sample_fhd_noSound.mp4 based on input/res/mute."
  (let* ((base (file-namestring input))
         (dot-pos (position #\. base :from-end t))
         (name (subseq base 0 dot-pos))
          ;; codec-infoのextがあればそれを,なければinputの拡張子を抽出
         (ext (or ext (subseq base dot-pos)))
         (res-suffix (if res (format nil "_~a" res) ""))
         (mute-suffix (if mute "_noSound" "")))
    (apply #'concatenate 'string (list name res-suffix mute-suffix ext))))

(defun string-replace (str from to)
  "Replace all instances of character FROM with TO in STR."
  (coerce
   (map 'list (lambda (c) (if (char= c from) to c)) str)
   'string))

(defun clean-args (args)
  "Normalize args: replace full-width spaces and downcase all."
  (mapcar (lambda (s)
            (string-downcase
             (visp::string-replace s #\u3000 #\Space)))
          args))

(defun encoder-available-p (name)
  "Return T if the given encoder name appears in `ffmpeg -encoders` output."
  (let ((output (uiop:run-program (list "ffmpeg" "-hide_banner" "-encoders")
                                  :output :string)))
    (search name output :test #'string=)))