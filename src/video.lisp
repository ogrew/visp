(in-package :visp)

(defun get-video-dims (input)
  "Return a cons (width . height) of the video using ffprobe."
  (let* ((cmd (list "ffprobe" "-v" "error" 
                    "-select_streams" "v:0"
                    "-show_entries" "stream=width,height"
                    "-of" "csv=p=0"
                    input))
         (output (string-trim '(#\Newline) (uiop:run-program cmd :output :string)))
         (parts (uiop:split-string output :separator ",")))
    (when (= (length parts) 2)
      (let ((w (parse-integer (first parts)))
            (h (parse-integer (second parts))))
        (cons w h)))))

(defun get-video-info (input)
  "Return an alist of video info: resolution, fps, duration, codec, audio codec, etc."
  (let* ((cmd (list "ffprobe" "-v" "error"
                    "-select_streams" "v"
                    "-show_entries" "stream=width,height,r_frame_rate,codec_name"
                    "-show_entries" "format=duration"
                    "-of" "default=noprint_wrappers=1:nokey=0"
                    input))
         (raw (uiop:run-program cmd :output :string)))

    (let* ((lines (uiop:split-string raw :separator '(#\Newline)))
           (info (loop for line in lines
                       for parts = (uiop:split-string line :separator "=")
                       when (= (length parts) 2)
                       collect (cons (first parts) (second parts)))))

      ;; 別途、音声ストリームの情報も取得
      (let* ((audio-cmd (list "ffprobe" "-v" "error"
                              "-select_streams" "a"
                              "-show_entries" "stream=codec_name"
                              "-of" "default=noprint_wrappers=1:nokey=1"
                              input))
             (audio-codec (string-trim '(#\Newline)
                                       (uiop:run-program audio-cmd :output :string))))
        (acons "audio_codec" audio-codec info)))))

(defun print-video-info (info)
  (let* ((codec    (cdr (assoc "codec_name" info :test #'string=)))
         (w        (cdr (assoc "width" info :test #'string=)))
         (h        (cdr (assoc "height" info :test #'string=)))
         (fps      (cdr (assoc "r_frame_rate" info :test #'string=)))
         (duration (cdr (assoc "duration" info :test #'string=)))
         (audio    (cdr (assoc "audio_codec" info :test #'string=)))
         (duration-str (if duration (format nil "~,2f sec" (read-from-string duration)) "N/A")))

    (format t "~%~a Input File Info~%" (visp:log-tag "info"))
    (format t "  Codec     : ~a~%" (or codec "N/A"))
    (format t "  Resolution: ~a~%" (if (and w h) (format nil "~ax~a" w h) "N/A"))
    (format t "  FPS       : ~a~%" (or fps "N/A"))
    (format t "  Duration  : ~a~%" duration-str)
    (format t "  Audio     : ~a~%~%" (or audio "none"))))






