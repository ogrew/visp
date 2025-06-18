(defpackage :visp.test.validate
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :parse-speed-float
                :validate-speed
                :validate-gif-mode))

(in-package :visp.test.validate)

(deftest parse-speed-float-tests
  (testing "Parses valid float strings"
    (ok (= (visp:parse-speed-float "2.0") 2.0))
    (ok (= (visp:parse-speed-float "0.5") 0.5))
    (ok (= (visp:parse-speed-float "1") 1.0))
    (ok (= (visp:parse-speed-float "3.14159") 3.14159)))

  (testing "Throws error for invalid strings"
    (ok (signals (visp:parse-speed-float "invalid")))
    (ok (signals (visp:parse-speed-float "abc")))
    (ok (signals (visp:parse-speed-float "")))))

(deftest validate-speed-tests
  (testing "Validates positive float values"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "2.0")
      (visp:validate-speed opts)
      (ok (= (visp:visp-options-speed opts) 2.0)))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "0.5")
      (visp:validate-speed opts)
      (ok (= (visp:visp-options-speed opts) 0.5)))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "1")
      (visp:validate-speed opts)
      (ok (= (visp:visp-options-speed opts) 1.0))))

  (testing "Handles NIL speed (no validation needed)"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) nil)
      (visp:validate-speed opts)
      (ok (null (visp:visp-options-speed opts))))))

(deftest validate-output-tests
  (testing "Validates output option with existing directory"
    ;; カレントディレクトリは存在するはず
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-output opts) "test_output.mp4")
      ;; エラーが発生しないことをテスト
      (visp:validate-output opts)
      (ok t)))
  
  (testing "Handles NIL output (no validation needed)"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-output opts) nil)
      (visp:validate-output opts)
      (ok (null (visp:visp-options-output opts))))))

(deftest validate-gif-mode-tests
  (testing "Accepts valid video formats"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      ;; エラーが発生しないことをテスト
      (visp:validate-gif-mode opts)
      (ok t))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mov")
      (visp:validate-gif-mode opts)
      (ok t))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.flv")
      (visp:validate-gif-mode opts)
      (ok t))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.avi")
      (visp:validate-gif-mode opts)
      (ok t))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.webm")
      (visp:validate-gif-mode opts)
      (ok t)))

  (testing "Rejects invalid file formats"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.jpg")
      (ok (signals (visp:validate-gif-mode opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.txt")
      (ok (signals (visp:validate-gif-mode opts)))))

  (testing "Rejects other options with --gif"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-output opts) "custom.gif")
      (ok (signals (visp:validate-gif-mode opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-res opts) "fhd")
      (ok (signals (visp:validate-gif-mode opts)))))

  (testing "Allows --dry-run with --gif"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-dry-run opts) t)
      ;; エラーが発生しないことをテスト
      (visp:validate-gif-mode opts)
      (ok t)))

  (testing "Rejects missing input file"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) nil)
      (ok (signals (visp:validate-gif-mode opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "")
      (ok (signals (visp:validate-gif-mode opts))))))