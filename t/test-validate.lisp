(defpackage :visp.test.validate
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :parse-speed-float
                :validate-speed))

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