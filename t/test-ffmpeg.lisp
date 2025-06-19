(defpackage :visp.test.ffmpeg
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :validate-output-path
                :cleanup-partial-output))

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
                            "muted.mp4")))))

  (testing "cmd with hflip filter"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :hflip t))
          (output "hflipped.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "hflip"
                            "hflipped.mp4")))))

  (testing "cmd with vflip filter"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :vflip t))
          (output "vflipped.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "vflip"
                            "vflipped.mp4")))))

  (testing "cmd with both hflip and vflip filters"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :hflip t
                                    :vflip t))
          (output "flipped.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "hflip,vflip"
                            "flipped.mp4")))))

  (testing "cmd with scale, hflip, and reverse (filter order)"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :scale '(1280 . 720)
                                    :hflip t
                                    :rev t))
          (output "complex.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "scale=1280:720,reverse,hflip"
                            "complex.mp4")))))

  (testing "cmd with speed filter"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :speed 2.0))
          (output "speed.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "setpts=PTS/2.0"
                            "speed.mp4")))))

  (testing "cmd with speed and other filters"
    (let ((opts (make-visp-options :input "input.mp4"
                                    :scale '(1920 . 1080)
                                    :speed 0.5
                                    :hflip t))
          (output "complex-speed.mp4"))
      (ok (equal (visp:build-cmd opts output)
                 '("ffmpeg" "-y" "-i" "input.mp4"
                            "-vf" "scale=1920:1080,setpts=PTS/0.5,hflip"
                            "complex-speed.mp4"))))))

(deftest validate-output-path-tests
  (testing "Validates existing directory paths"
    ;; カレントディレクトリは存在するはず
    (ok (not (signals error (validate-output-path "test-output.mp4"))))
    (ok (not (signals error (validate-output-path "./test-output.mp4")))))
  
  ;; NOTE: uiop:quitを呼ぶ関数のテストは現在のテストフレームワークでは困難
  ;; 実際のエラーケースのテストは統合テストで実行する
  )

(deftest cleanup-partial-output-tests
  (testing "Cleans up small/empty files"
    ;; 小さなテストファイルを作成
    (let ((test-file "test-partial.mp4"))
      ;; 空ファイルを作成
      (with-open-file (stream test-file :direction :output 
                             :if-exists :supersede)
        (write-string "" stream))
      
      ;; クリーンアップが動作することを確認
      (ok (probe-file test-file))
      (cleanup-partial-output test-file)
      (ok (not (probe-file test-file)))))
  
  (testing "Preserves normal-sized files"
    ;; 通常サイズのファイルは削除されない
    (let ((test-file "test-normal.mp4"))
      ;; 十分なサイズのファイルを作成
      (with-open-file (stream test-file :direction :output 
                             :if-exists :supersede)
        (write-string (make-string 2048 :initial-element #\a) stream))
      
      (ok (probe-file test-file))
      (cleanup-partial-output test-file)
      (ok (probe-file test-file))
      
      ;; テスト後のクリーンアップ
      (delete-file test-file))))