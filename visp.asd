(defsystem "visp"
  :description "Minimal ffmpeg wrapper CLI tool written in Common Lisp"
  :version "0.1.0"
  :author "ogrew"
  :license "MIT"
  :depends-on (:uiop)
  :components ((:file "package")
               (:file "util")
               (:file "main")))
