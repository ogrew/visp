(defpackage :visp.test.validate
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :parse-float
                :validate-speed))

(in-package :visp.test.validate)

(deftest parse-float-tests
  (testing "Parses valid float strings"
    (ok (= (visp:parse-float "2.0") 2.0))
    (ok (= (visp:parse-float "0.5") 0.5))
    (ok (= (visp:parse-float "1") 1.0))
    (ok (= (visp:parse-float "3.14159") 3.14159)))

  (testing "Returns NIL for invalid strings"
    (ok (null (visp:parse-float "invalid")))
    (ok (null (visp:parse-float "abc")))
    (ok (null (visp:parse-float "")))))

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