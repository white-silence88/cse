;; answer.lisp
(in-package #:cse)

;; jsonify
;;
;; Descriprion:
;;   procedure for convert raw data (alist) to JSON string
;; Params:
;;   raw-data  [List]  data for convert to JSON
;; Returns:
;;   JSON string
(defun jsonify (raw-data)
  (jonathan:to-json raw-data :from :alist))

;; get-answer
;;
;;
;; Descriprion:
;;   procedure for get answer correct answer list
;; Params:
;;   code           [Integer]   answer code
;;   content-type   [String]    content type
;;   body-list      [List]      data for answer in alist format
(defun get-answer (code content-type body-list)
  (let
      ((json-body (jsonify body-list)))
    `(,code (:content-type ,content-type) (,json-body))))
