(defpackage :visp.test.util
  (:use :cl :rove))
(in-package :visp.test.util)

(deftest input-extension-tests
  (testing "Extract file extension correctly"
    (ok (string= (visp::input-extension "video.mp4") ".mp4"))
    (ok (string= (visp::input-extension "dir/clip.MOV") ".mov"))
    (ok (string= (visp::input-extension "noext") ""))
    (ok (string= (visp::input-extension "archive.tar.gz") ".gz"))))

(deftest string-replace-tests
  (testing "Replaces characters correctly"
    (ok (string= (visp:string-replace "a-b-c" #\- #\_) "a_b_c"))
    (ok (string= (visp:string-replace "hello world" #\space #\_) "hello_world"))
    (ok (string= (visp:string-replace "no-changes" #\x #\_) "no-changes"))
    (ok (string= (visp:string-replace "" #\a #\b) ""))))

(deftest parse-frame-rate-tests
  (testing "Parses valid frame rate strings"
    (ok (= (visp:parse-frame-rate "30") 30.0))
    (ok (= (visp:parse-frame-rate "30000/1001") (/ 30000 1001))))

  (testing "Handles invalid input gracefully"
    (ok (null (visp:parse-frame-rate "not-a-number")))
    (ok (null (visp:parse-frame-rate "12/abc")))
    (ok (null (visp:parse-frame-rate "")))))

(deftest resolution-from-key-tests
  (testing "Returns resolution tuple for valid keys"
    (ok (equal (cdr (visp:resolution-from-key "fhd")) '(1920 . 1080)))
    (ok (equal (cdr (visp:resolution-from-key "4k")) '(3840 . 2160))))

  (testing "Returns NIL for unknown keys"
    (ok (null (visp:resolution-from-key "unknown")))
    (ok (null (visp:resolution-from-key "")))))

(deftest parse-dimensions-tests
  (testing "Parses valid WxH strings"
    (ok (equal (visp:parse-dimensions "1920x1080") '(1920 . 1080)))
    (ok (equal (visp:parse-dimensions "1280x-1") '(1280 . -1))))

  (testing "Returns NIL for malformed strings"
    (ok (null (visp:parse-dimensions "1920*1080")))
    (ok (null (visp:parse-dimensions "x720")))))

(deftest codec-info-from-key-tests
  (testing "Returns correct plist for known codecs"
    (ok (equalp (visp:codec-info-from-key "h264")
                '(:ENCODER "libx264" :EXT ".mp4")))
    (ok (equalp (visp:codec-info-from-key "hap")
                '(:ENCODER "hap" :EXT ".mov" :PIX_FMT "yuva420p"))))

  (testing "Returns NIL for unknown codec keys"
    (ok (null (visp:codec-info-from-key "unknown")))
    (ok (null (visp:codec-info-from-key "")))))

(deftest clean-args-tests
  (testing "Handles normal ASCII strings"
    (ok (equal (visp:clean-args '("Hello" "WORLD")) '("hello" "world"))))

  (testing "Replaces full-width spaces with ASCII spaces"
    (ok (equal (visp:clean-args '("foo　bar")) '("foo bar")))) ; ← "　" は U+3000

  (testing "Handles mix of full-width space and uppercase"
    (ok (equal (visp:clean-args '("ABC　DEF")) '("abc def"))))

  (testing "Handles empty list"
    (ok (equal (visp:clean-args '()) '())))

  (testing "Handles empty string"
    (ok (equal (visp:clean-args '("")) '("")))))


(deftest output-path-in-same-directory-tests
  (testing "Output path is correctly generated in same directory"
    ;; Case 1: 通常のパス
    (ok (string= (visp:output-path-in-same-directory "/Users/ogrew/videos/video01.mp4" "video01_fhd.mp4")
                 "/Users/ogrew/videos/video01_fhd.mp4"))

    ;; Case 2: ディレクトリの末尾がスラッシュでない場合
    (ok (string= (visp:output-path-in-same-directory "/Users/ogrew/videos/video02" "video02_fhd.mp4")
                 "/Users/ogrew/videos/video02_fhd.mp4"))

    ;; Case 3: カレントディレクトリ (相対パス)
    (ok (string= (visp:output-path-in-same-directory "video03.mp4" "video03_fhd.mp4")
                 "video03_fhd.mp4"))))

(deftest generate-output-filename-with-flip-tests
  (testing "Filename generation includes flip suffixes"
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-hflip opts) t)
      (ok (string= (visp:generate-output-filename opts) "test_HFlip.mp4")))
    
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-vflip opts) t)
      (ok (string= (visp:generate-output-filename opts) "test_VFlip.mp4")))
    
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-hflip opts) t)
      (setf (visp:visp-options-vflip opts) t)
      (ok (string= (visp:generate-output-filename opts) "test_HFlip_VFlip.mp4")))
    
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-res opts) "fhd")
      (setf (visp:visp-options-hflip opts) t)
      (setf (visp:visp-options-mute opts) t)
      (ok (string= (visp:generate-output-filename opts) "test_fhd_noSound_HFlip.mp4"))))

(deftest generate-output-filename-with-speed-tests
  (testing "Filename generation includes speed suffixes"
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-speed opts) 2.0)
      (ok (string= (visp:generate-output-filename opts) "test_2.0xSpeed.mp4")))
    
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-speed opts) 0.5)
      (ok (string= (visp:generate-output-filename opts) "test_0.5xSpeed.mp4")))
    
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-res opts) "fhd")
      (setf (visp:visp-options-speed opts) 1.5)
      (setf (visp:visp-options-mute opts) t)
      (ok (string= (visp:generate-output-filename opts) "test_fhd_noSound_1.5xSpeed.mp4")))))

(deftest generate-output-filename-with-output-option-tests
  (testing "Custom output filename takes precedence over auto-generation"
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-output opts) "custom.mp4")
      (ok (string= (visp:generate-output-filename opts) "custom.mp4")))
    
    ;; --outputが指定されている場合、他のオプションは無視される
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-output opts) "custom.mp4")
      (setf (visp:visp-options-res opts) "fhd")
      (setf (visp:visp-options-mute opts) t)
      (setf (visp:visp-options-speed opts) 2.0)
      (ok (string= (visp:generate-output-filename opts) "custom.mp4")))))

(deftest generate-gif-output-filename-with-output-option-tests
  (testing "GIF output filename with --output option"
    ;; --outputが指定されていない場合の従来動作
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (ok (string= (visp:generate-gif-output-filename opts) "test.gif")))
    
    ;; --outputが指定されている場合
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-output opts) "custom.gif")
      (ok (string= (visp:generate-gif-output-filename opts) "custom.gif")))
    
    ;; --outputが指定されていない場合
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-input opts) "test.mp4")
      (ok (string= (visp:generate-gif-output-filename opts) "test.gif")))))

(deftest generate-merge-output-filename-with-output-option-tests
  (testing "Merge output filename with --output option"
    ;; --outputが指定されていない場合の従来動作
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-merge-files opts) '("file1.mp4" "file2.mp4"))
      (ok (string= (visp:generate-merge-output-filename opts) "file1_merged.mp4")))
    
    ;; --outputが指定されている場合
    (let ((opts (visp:make-visp-options)))
      (setf (visp:visp-options-merge-files opts) '("file1.mp4" "file2.mp4"))
      (setf (visp:visp-options-output opts) "custom_merged.mp4")
      (ok (string= (visp:generate-merge-output-filename opts) "custom_merged.mp4"))))))
