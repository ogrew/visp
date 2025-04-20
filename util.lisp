(in-package :visp)

(defparameter *resolution-map*
  '(("hd"     . (1280 . 720))
    ("720p"   . (1280 . 720))
    ("fhd"    . (1920 . 1080))
    ("1080p"  . (1920 . 1080))
    ("2k"     . (2560 . 1440))
    ("4k"     . (3840 . 2160))
    ("8k"     . (7680 . 4320))))

(defun resolution-from-key (key)
  "Return (width . height) if key is valid; otherwise NIL"
  (assoc key *resolution-map* :test #'string-equal))

(defun generate-output-filename (input res mute)
  "Return a filename like sample_fhd_noSound.mp4 based on input/res/mute."
  (let* ((base (file-namestring input))
         (dot-pos (position #\. base :from-end t))
         (name (subseq base 0 dot-pos))
         (ext (subseq base dot-pos))
         (res-suffix (if res (format nil "_~a" res) ""))
         (mute-suffix (if mute "_noSound" "")))
    (apply #'concatenate 'string (list name res-suffix mute-suffix ext))))

(defun clean-args (args)
  "Replace all full-width spaces in each arg with normal spaces."
  (mapcar (lambda (s)
            (string-replace s #\u3000 #\Space))
          args))

(defun string-replace (str from to)
  "Replace all instances of character FROM with TO in STR."
  (coerce
   (map 'list (lambda (c) (if (char= c from) to c)) str)
   'string))