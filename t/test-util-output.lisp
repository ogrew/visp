(defpackage :visp.test.util-output
  (:use :cl :rove)
  (:import-from :visp
                :generate-merge-output-filename
                :make-visp-options))

(in-package :visp.test.util-output)

(deftest generate-merge-output-filename-tests
  (testing "Filename is generated from first merge input"
    (let ((opts (visp:make-visp-options
                 :merge-files '("movie01.mp4" "movie02.mp4"))))
      (ok (string= (visp:generate-merge-output-filename opts)
                   "movie01_merged.mp4")))))

(deftest generate-output-filename-tests
  (testing "Only input, no options"
    (let ((opts (make-visp-options :input "sample.mp4")))
      (ok (string= (visp:generate-output-filename opts)
                   "sample.mp4"))))

  (testing "With resolution"
    (let ((opts (make-visp-options :input "clip.mp4" :res "720p")))
      (ok (string= (visp:generate-output-filename opts)
                   "clip_720p.mp4"))))

  (testing "With mute and reverse"
    (let ((opts (make-visp-options :input "movie.mov" :mute t :rev t)))
      (ok (string= (visp:generate-output-filename opts)
                   "movie_noSound_Reverse.mov"))))

  (testing "With fps and half"
    (let ((opts (make-visp-options :input "rec.mp4" :fps 60 :half t)))
      (ok (string= (visp:generate-output-filename opts)
                   "rec_60fps_Half.mp4"))))

  (testing "All options"
    (let ((opts (make-visp-options :input "full.mov"
                                   :res "4k"
                                   :fps 30
                                   :mute t
                                   :rev t
                                   :half t
                                   :mono t
                                   :repeat 3)))
      (ok (string= (visp:generate-output-filename opts)
                   "full_4k_30fps_noSound_Reverse_Half_Gray_x3.mov"))))

  (testing "With ext override"
    (let ((opts (make-visp-options :input "base.mov" :res "2k")))
      (ok (string= (visp:generate-output-filename opts ".mkv")
                   "base_2k.mkv")))))