(in-package :visp)

(defun colorize (text code)
  (format nil "~c[~am~a~c[0m" #\Esc code text #\Esc))

(defun log-tag (type)
  (cond
    ((string= type "info")     (colorize "[INFO]" "34;1"))    ; 青
    ((string= type "dry-run")  (colorize "[DRY-RUN]" "33;1")) ; 黄
    ((string= type "warn")     (colorize "[WARN]" "35;1"))    ; 紫
    ((string= type "error")    (colorize "[ERROR]" "31;1"))   ; 赤
    (t type)))  ; fallback
