(in-package :visp)

;; ベースとなる例外クラス
(define-condition visp-error (error)
  ((message :initarg :message 
            :reader visp-error-message
            :documentation "エラーメッセージ")
   (context :initarg :context 
            :reader visp-error-context
            :initform nil
            :documentation "エラーが発生したコンテキスト（入力値など）"))
  (:documentation "VISPアプリケーションのベースエラークラス")
  (:report (lambda (condition stream)
             (format stream "VISP Error: ~A" 
                     (visp-error-message condition)))))

;; バリデーション関連のエラー
(define-condition visp-validation-error (visp-error)
  ()
  (:documentation "入力値やオプションのバリデーションエラー")
  (:report (lambda (condition stream)
             (format stream "Validation Error: ~A~@[ (context: ~A)~]"
                     (visp-error-message condition)
                     (visp-error-context condition)))))

;; オプション関連のエラー
(define-condition visp-option-error (visp-validation-error)
  ((option-name :initarg :option-name
                :reader visp-option-error-option-name
                :initform nil
                :documentation "エラーが発生したオプション名"))
  (:documentation "コマンドラインオプションのエラー")
  (:report (lambda (condition stream)
             (format stream "Option Error~@[ in --~A~]: ~A~@[ (value: ~A)~]"
                     (visp-option-error-option-name condition)
                     (visp-error-message condition)
                     (visp-error-context condition)))))

;; ファイル関連のエラー
(define-condition visp-file-error (visp-validation-error)
  ((file-path :initarg :file-path
              :reader visp-file-error-file-path
              :initform nil
              :documentation "エラーが発生したファイルパス"))
  (:documentation "ファイル操作のエラー")
  (:report (lambda (condition stream)
             (format stream "File Error~@[ (~A)~]: ~A"
                     (visp-file-error-file-path condition)
                     (visp-error-message condition)))))

;; ffmpeg実行関連のエラー
(define-condition visp-ffmpeg-error (visp-error)
  ((command :initarg :command
            :reader visp-ffmpeg-error-command
            :initform nil
            :documentation "実行に失敗したffmpegコマンド")
   (exit-code :initarg :exit-code
              :reader visp-ffmpeg-error-exit-code
              :initform nil
              :documentation "ffmpegの終了コード"))
  (:documentation "ffmpeg実行時のエラー")
  (:report (lambda (condition stream)
             (format stream "FFmpeg Error~@[ (exit code: ~A)~]: ~A"
                     (visp-ffmpeg-error-exit-code condition)
                     (visp-error-message condition)))))

;; ヘルパー関数: 簡単に例外を発生させるためのユーティリティ

(defun error-option (message &key option-name context)
  "オプションエラーを発生させる"
  (error 'visp-option-error
         :message message
         :option-name option-name
         :context context))

(defun error-file (message &key file-path context)
  "ファイルエラーを発生させる"
  (error 'visp-file-error
         :message message
         :file-path file-path
         :context context))

(defun error-validation (message &key context)
  "バリデーションエラーを発生させる"
  (error 'visp-validation-error
         :message message
         :context context))

(defun error-ffmpeg (message &key command exit-code context)
  "ffmpegエラーを発生させる"
  (error 'visp-ffmpeg-error
         :message message
         :command command
         :exit-code exit-code
         :context context))