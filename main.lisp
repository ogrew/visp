(in-package :visp)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    ;; ここで args を解析して ffmpegコマンド生成
    ;; ffmpeg.lispの関数を使う
    (format t "Arguments: ~a~%" args)))