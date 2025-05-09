(defsystem "visp"
  :description "Minimal ffmpeg wrapper CLI tool written in Common Lisp"
  :version "0.2.0"
  :author "ogrew"
  :license "MIT"
  :depends-on (:uiop)
  :components ((:module "src"
                 :components
                 ((:file "package")
                  (:file "const")
                  (:file "log")
                  (:file "help")
                  (:file "options")
                  (:file "util")
                  (:file "video")
                  (:file "ffmpeg")
                  (:file "validate")
                  (:file "main")))))

(defsystem "visp/test"
  :depends-on (:visp :rove)
  :components ((:module "t"
                :components
                ((:file "test-ffmpeg")
                 (:file "test-util")
                 (:file "test-util-output"))))
  :description "Test suite for visp")