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
