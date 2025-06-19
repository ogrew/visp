(defpackage :visp.test.validate
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :parse-number
                :validate-speed
                :validate-gif-mode
                :visp-option-error))

(in-package :visp.test.validate)

(deftest parse-number-tests
  (testing "Parses valid number strings"
    (ok (= (visp:parse-number "2.0") 2.0))
    (ok (= (visp:parse-number "0.5") 0.5))
    (ok (= (visp:parse-number "1") 1.0))
    (ok (= (visp:parse-number "3.14159") 3.14159)))

  (testing "Returns NIL for invalid strings"
    (ok (null (visp:parse-number "invalid")))
    (ok (null (visp:parse-number "abc")))
    (ok (null (visp:parse-number "")))))

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
      (ok (null (visp:visp-options-speed opts)))))

  (testing "Throws visp-option-error for invalid speed values"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "invalid")
      (ok (signals visp-option-error (validate-speed opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "-1.5")
      (ok (signals visp-option-error (validate-speed opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "0")
      (ok (signals visp-option-error (validate-speed opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-speed opts) "abc")
      (ok (signals visp-option-error (validate-speed opts)))))))

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

  ;; NOTE: Error case tests have been temporarily removed due to (uiop:quit 1) 
  ;; incompatibility with test framework. These will be added back when 
  ;; validation functions are refactored to use exceptions instead of process exit.
  ;; See CLAUDE.md "テストコード全体の改修とエラーケーステストの追加" for details.

  (testing "Allows --dry-run with --gif"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-dry-run opts) t)
      ;; エラーが発生しないことをテスト
      (visp:validate-gif-mode opts)
      (ok t))))