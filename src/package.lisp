(defpackage :visp
  (:use :cl :uiop)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

   ;; const.lisp
   :+resolution-map+
   :+codec-map+
   :+pixfmt-name-map+
   :+allowed-input-extensions+
   +gif-scale+

   ;; main.lisp
   :main

   ;; help.lisp
   :print-help

  ;; validate.lisp
  :validate-merge-files
  :validate-gif-mode
  :validate-input
  :validate-reverse
  :validate-repeat
  :validate-resolution
  :validate-half
  :validate-fps
  :validate-codec
  :validate-mono
  :validate-speed
  :validate-options
  :dispatch-validation

   ;; util.lisp
   :input-extension
   :parse-float
   :safe-parse-float
   :parse-frame-rate
   :resolution-from-key
   :parse-dimensions
   :codec-info-from-key
   :generate-output-filename
   :generate-merge-output-filename
   :generate-gif-output-filename
   :string-replace
   :clean-args
   :string-prefix-p
   :output-path-in-same-directory

   ;; video.lisp
   :get-video-dims
   :get-video-fps
   :get-video-info
   :print-video-info
   :all-have-audio-p

   ;; ffmpeg.lisp
   :run-cmd
   :build-gif-cmd
   :build-concat-filter
   :build-merge-cmd
   :build-cmd
   :encoder-available-p
   :ffmpeg-available-p

   ;; log.lisp
   :log-tag
   :colorize

   ;; options.lisp
   :parse-args-to-options
   :make-visp-options
   :visp-options-input
   :visp-options-res
   :visp-options-scale
   :visp-options-codec
   :visp-options-codec-info
   :visp-options-fps
   :visp-options-repeat
   :visp-options-mute
   :visp-options-half
   :visp-options-mono
   :visp-options-hflip
   :visp-options-vflip
   :visp-options-speed
   :visp-options-rev
   :visp-options-dry-run
   :visp-options-merge-files
   :visp-options-gif
   :visp-options-output
   :visp-options-batch-files
   ))