(defsystem "visp"
  :description "Minimal ffmpeg wrapper CLI tool written in Common Lisp"
  :version "0.2.0"
  :author "ogrew"
  :license "MIT"
  :depends-on (:uiop)
  :components ((:module "src"
                 :components
                 ((:file "package")
                  (:file "const" :depends-on ("package"))
                  (:file "log" :depends-on ("package"))
                  (:file "help" :depends-on ("package"))
                  (:file "exceptions" :depends-on ("package"))
                  (:file "options" :depends-on ("package"))
                  (:file "util" :depends-on ("package"))
                  (:file "video" :depends-on ("package" "util"))
                  (:file "ffmpeg" :depends-on ("package" "util" "video"))
                  (:file "validate" :depends-on ("package" "exceptions" "util" "video"))
                  (:file "main" :depends-on ("package" "exceptions" "options" "util" "video" "ffmpeg" "validate" "help" "log"))))))

(defsystem "visp/test"
  :depends-on (:visp :rove)
  :components ((:module "t"
                :components
                ((:file "test-exceptions")
                 (:file "test-ffmpeg")
                 (:file "test-util")
                 (:file "test-util-output")
                 (:file "test-validate"))))
  :description "Test suite for visp")