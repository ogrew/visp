(defpackage :visp
  (:use :cl :uiop)
  (:documentation "Minimal ffmpeg wrapper CLI tool written in Common Lisp.")
  (:export

   ;; main.lisp
   :main

   ;; cli.lisp
   :print-help
   :validate-options

   ;; ffmpeg.lisp
   :build-cmd
   :encoder-available-p
   :ffmpeg-available-p

   ;; util.lisp
   :resolution-from-key
   :codec-info-from-key
   :generate-output-filename
   :string-replace
   :clean-args

   ;; log.lisp
   :log-tag

   ;; options.lisp
   :parse-args-to-options
   :make-visp-options
   :visp-options-input
   :visp-options-res
   :visp-options-scale
   :visp-options-codec
   :visp-options-codec-info
   :visp-options-fps
   :visp-options-loop
   :visp-options-mute
   :visp-options-dry-run
   ))