(defpackage :visp.test.ffmpeg
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options))

(in-package :visp.test.ffmpeg)

(deftest build-cmd-tests
  (testing "basic cmd with only input and output"
    (let ((opts (make-visp-options :input "input.mp4"))
          (output "out.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4" "out.mp4")))))

  (testing "cmd with scale (resolution)"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :scale '(1920 . 1080)))
          (output "scaled.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "scale=1920:1080"
                            "scaled.mp4")))))

  (testing "cmd with scale and reverse"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :scale '(1280 . 720)
                                    :rev t))
          (output "scaled-reversed.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "scale=1280:720,reverse"
                            "scaled-reversed.mp4")))))


  (testing "cmd with mute and fps"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :fps 30
                                    :mute t))
          (output "muted.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-an"
                            "-r" "30"
                            "muted.mp4"))))))