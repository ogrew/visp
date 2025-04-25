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
    (ok (= (visp:parse-frame-rate "30000/1001") (/ 30000.0 1001))))

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
