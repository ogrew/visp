(defpackage :visp
  (:use :cl :uiop)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

   ;; main.lisp
   :main

   ;; help.lisp
   :print-help

  ;; validate.lisp
  :validate-input
  :validate-reverse
  :validate-repeat
  :validate-resolution
  :validate-half
  :validate-fps
  :validate-codec
  :validate-mono
  :validate-options

   ;; util.lisp
   :input-extension
   :resolution-from-key
   :codec-info-from-key
   :generate-output-filename
   :string-replace
   :clean-args

   ;; video.lisp
   :get-video-dims
   :get-video-info
   :print-video-info

   ;; ffmpeg.lisp
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
   :visp-options-rev
   :visp-options-dry-run
   ))