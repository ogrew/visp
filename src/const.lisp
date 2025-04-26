(in-package :visp)

(defparameter +resolution-map+
  '(("360p"   . (640 . 360))
    ("480p"   . (854 . 480))
    ("hd"     . (1280 . 720))
    ("720p"   . (1280 . 720))
    ("fhd"    . (1920 . 1080))
    ("1080p"  . (1920 . 1080))
    ("2k"     . (2560 . 1440))
    ("4k"     . (3840 . 2160))
    ("8k"     . (7680 . 4320))))

(defparameter +codec-map+
  '(("h264"  . (:encoder "libx264"    :ext ".mp4"))
    ("h265"  . (:encoder "libx265"    :ext ".mp4"))
    ("hevc"  . (:encoder "libx265"    :ext ".mp4")) ; alias
    ("prores" . (:encoder "prores_ks" :ext ".mov" :pix_fmt "yuv422p10le"))
    ("hap"    . (:encoder "hap"       :ext ".mov" :pix_fmt "yuva420p"))
    ("vp9"    . (:encoder "libvpx-vp9" :ext ".webm"))
    ("vp8"    . (:encoder "libvpx"     :ext ".webm"))))

(defparameter +pixfmt-name-map+
  '(("yuv422p10le" . "ProRes 422 HQ / LT")
    ("yuv444p10le" . "ProRes 4444")
    ("yuva420p"    . "HAP Alpha")
    ("yuv420p"     . "H.264 Default")
    ;; 必要に応じて拡張
    ))

(defparameter +allowed-input-extensions+
  '(".mp4" ".mov" ".flv" ".avi" ".webm"))

(defparameter +gif-scale+ "640:-1")
