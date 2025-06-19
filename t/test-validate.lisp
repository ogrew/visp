(defpackage :visp.test.validate
  (:use :cl :rove)
  (:import-from :visp
                :make-visp-options
                :parse-number
                :validate-speed
                :validate-input
                :validate-reverse
                :validate-repeat
                :validate-resolution
                :validate-half
                :validate-fps
                :validate-gif-mode
                :visp-option-error
                :visp-file-error))

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

  (testing "Error when no input provided in GIF mode"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      ;; input is nil by default
      (ok (signals visp-option-error (validate-gif-mode opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "")
      (ok (signals visp-option-error (validate-gif-mode opts)))))
  
  (testing "Error when unsupported file format in GIF mode"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.txt")
      (ok (signals visp-file-error (validate-gif-mode opts)))))
  
  (testing "Error when --gif used with other options"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-res opts) "hd")
      (ok (signals visp-option-error (validate-gif-mode opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-codec opts) "h264")
      (ok (signals visp-option-error (validate-gif-mode opts)))))

  (testing "Allows --dry-run with --gif"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-gif opts) t)
      (setf (visp:visp-options-input opts) "test.mp4")
      (setf (visp:visp-options-dry-run opts) t)
      ;; エラーが発生しないことをテスト
      (visp:validate-gif-mode opts)
      (ok t))))

(deftest validate-input-error-tests
  (testing "Error when no input provided"
    (let ((opts (make-visp-options)))
      ;; input is nil by default
      (ok (signals visp-option-error (validate-input opts)))))
  
  (testing "Error when input file does not exist"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-input opts) "/nonexistent/file.mp4")
      (ok (signals visp-file-error (validate-input opts)))))
  
  (testing "Error when input directory does not exist"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-input opts) "/nonexistent/directory/")
      (ok (signals visp-file-error (validate-input opts))))))

(deftest validate-reverse-error-tests
  (testing "Error when --reverse and --loop used together"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-rev opts) t)
      (setf (visp:visp-options-repeat opts) 2)
      (setf (visp:visp-options-input opts) "test.mp4")
      (ok (signals visp-option-error (validate-reverse opts)))))
  
  (testing "Error when --reverse used with unsupported extension"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-rev opts) t)
      (setf (visp:visp-options-input opts) "test.avi")
      (ok (signals visp-option-error (validate-reverse opts))))))

(deftest validate-repeat-error-tests
  (testing "Error when --loop is not a positive integer"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-repeat opts) "invalid")
      (ok (signals visp-option-error (validate-repeat opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-repeat opts) "0")
      (ok (signals visp-option-error (validate-repeat opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-repeat opts) "-1")
      (ok (signals visp-option-error (validate-repeat opts))))))

(deftest validate-resolution-error-tests
  (testing "Error when --res and --half used together"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-res opts) "fhd")
      (setf (visp:visp-options-half opts) t)
      (ok (signals visp-option-error (validate-resolution opts)))))
  
  (testing "Error when --res has unsupported value"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-res opts) "unsupported-resolution")
      (ok (signals visp-option-error (validate-resolution opts))))))

(deftest validate-fps-error-tests
  (testing "Error when --fps is not a positive integer"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-fps opts) "invalid")
      (ok (signals visp-option-error (validate-fps opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-fps opts) "0")
      (ok (signals visp-option-error (validate-fps opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-fps opts) "-30")
      (ok (signals visp-option-error (validate-fps opts))))))

(deftest validate-output-error-tests
  (testing "Error when output directory does not exist"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-output opts) "/nonexistent/directory/output.mp4")
      (ok (signals visp-file-error (validate-output opts))))))

(deftest validate-codec-error-tests
  (testing "Error when unsupported codec specified"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-codec opts) "unsupported-codec")
      (ok (signals visp-option-error (validate-codec opts))))))

(deftest validate-mono-error-tests
  (testing "Error when --mono used with unsupported codecs"
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-mono opts) t)
      (setf (visp:visp-options-codec opts) "prores")
      (ok (signals visp-option-error (validate-mono opts))))
    
    (let ((opts (make-visp-options)))
      (setf (visp:visp-options-mono opts) t)
      (setf (visp:visp-options-codec opts) "hap")
      (ok (signals visp-option-error (validate-mono opts))))))