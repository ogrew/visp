(in-package :visp)

(defun colorize (text code)
  (format nil "~c[~am~a~c[0m" #\Esc code text #\Esc))

(defun log-tag (type)
  (cond
    ((string= type "info")     (colorize "[INFO]" "34;1"))
    ((string= type "dry-run")  (colorize "[DRY-RUN]" "33;1"))
    ((string= type "warn")     (colorize "[WARN]" "35;1"))
    ((string= type "error")    (colorize "[ERROR]" "31;1"))
    (t type)))
