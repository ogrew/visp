(defpackage :visp.test.exceptions
  (:use :cl :rove)
  (:import-from :visp
                ;; 例外クラス
                :visp-error
                :visp-error-message
                :visp-error-context
                :visp-validation-error
                :visp-option-error
                :visp-option-error-option-name
                :visp-file-error
                :visp-file-error-file-path
                :visp-ffmpeg-error
                :visp-ffmpeg-error-command
                :visp-ffmpeg-error-exit-code
                ;; ヘルパー関数
                :error-option
                :error-file
                :error-validation
                :error-ffmpeg))

(in-package :visp.test.exceptions)

(deftest visp-error-basic-tests
  (testing "visp-error basic functionality"
    (let ((err (make-condition 'visp-error 
                               :message "Test error"
                               :context "test-context")))
      (ok (string= (visp-error-message err) "Test error"))
      (ok (string= (visp-error-context err) "test-context")))
    
    (let ((err (make-condition 'visp-error :message "Simple error")))
      (ok (string= (visp-error-message err) "Simple error"))
      (ok (null (visp-error-context err))))))

(deftest visp-option-error-tests
  (testing "visp-option-error functionality"
    (let ((err (make-condition 'visp-option-error
                               :message "Invalid value"
                               :option-name "speed"
                               :context "abc")))
      (ok (string= (visp-error-message err) "Invalid value"))
      (ok (string= (visp-option-error-option-name err) "speed"))
      (ok (string= (visp-error-context err) "abc")))))

(deftest visp-file-error-tests
  (testing "visp-file-error functionality"
    (let ((err (make-condition 'visp-file-error
                               :message "File not found"
                               :file-path "/path/to/file.mp4")))
      (ok (string= (visp-error-message err) "File not found"))
      (ok (string= (visp-file-error-file-path err) "/path/to/file.mp4")))))

(deftest visp-ffmpeg-error-tests
  (testing "visp-ffmpeg-error functionality"
    (let ((err (make-condition 'visp-ffmpeg-error
                               :message "Command failed"
                               :command '("ffmpeg" "-i" "input.mp4")
                               :exit-code 1)))
      (ok (string= (visp-error-message err) "Command failed"))
      (ok (equal (visp-ffmpeg-error-command err) '("ffmpeg" "-i" "input.mp4")))
      (ok (= (visp-ffmpeg-error-exit-code err) 1)))))

(deftest error-helper-functions-tests
  (testing "error-option helper"
    (ok (signals visp-option-error
                 (error-option "Invalid speed" :option-name "speed" :context "abc"))))
  
  (testing "error-file helper"
    (ok (signals visp-file-error
                 (error-file "File not found" :file-path "/nonexistent.mp4"))))
  
  (testing "error-validation helper"
    (ok (signals visp-validation-error
                 (error-validation "Invalid input" :context "test"))))
  
  (testing "error-ffmpeg helper"
    (ok (signals visp-ffmpeg-error
                 (error-ffmpeg "Command failed" :command '("ffmpeg") :exit-code 1)))))

(deftest exception-hierarchy-tests
  (testing "Exception inheritance hierarchy"
    ;; visp-validation-error is a subtype of visp-error
    (ok (signals visp-error
                 (error-validation "test")))
    
    ;; visp-option-error is a subtype of visp-validation-error
    (ok (signals visp-validation-error
                 (error-option "test")))
    
    ;; visp-file-error is a subtype of visp-validation-error
    (ok (signals visp-validation-error
                 (error-file "test")))
    
    ;; visp-ffmpeg-error is a subtype of visp-error
    (ok (signals visp-error
                 (error-ffmpeg "test")))))