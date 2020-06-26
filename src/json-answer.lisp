;; json-answer.lisp
(in-package :cse)
;; Define parameter name of code field
(defparameter *code-field* "code")
;; Define parameter name of message field
(defparameter *message-field* "message")
;; Define parameter name of not found key
(defparameter *not-found-config-key* "notFound")

;; answer->>jsonify
;;
;; Descriprion:
;;   procedure for convert raw data (alist) to JSON string
;; Params:
;;   raw-data  [List]  data for convert to JSON
;; Returns:
;;   JSON string
(defun answer->>jsonify (raw-data)
  (jonathan:to-json raw-data :from :alist))

;; answer/get
;;
;;
;; Descriprion:
;;   procedure for get answer correct answer list
;; Params:
;;   code           [Integer]   answer code
;;   content-type   [String]    content type
;;   body-list      [List]      data for answer in alist format
(defun answer/get (code content-type body-list)
  (let
      ((body (answer->>jsonify body-list)))
    `(,code (:content-type ,content-type) (,body))))

;; body-list/errors
;;
;;
;; Descriprion:
;;   procedure for build list of data for error answers
;; Params:
;;   config   [List]   list of config
;; Returns:
;;   alist with data
(defun body-list/errors (config)
  (let
      ((message (config/get *message-field* config)))
    (list (cons "message" message))))

;; errors/from-config
;;
;;
;; Descriprion:
;; Params:
;;   config         [List]      list of config
;;   content-type   [String]    content type
;;   property-name  [String]    name of property in config
;; Returns:
;;   list of answer
(defun errors/from-config (config content-type property-name)
  (let*
      ((property-config (config/get property-name config))
       (code (config/get *code-field* property-config))
       (body-list (body-list/errors property-config)))
    (answer/get code content-type body-list)))

;; answers/errors->notFound
;;
;;
;; Descriprion:
;;   prcedure for get standart error "Not Found" (404)
;; Params:
;;   client-errors   [List]      list of configs client errors
;;   content-type    [String]    content type
;; Return:
;;   answer list
(defun answers/errors->notFound (client-errors content-type)
  (errors/from-config client-errors content-type *not-found-config-key*))

;; body-list/success
;;
;;
;; Description:
;;   procedure for get body list for success result
;; Params:
;;   config   [List]   list of config
;; Returns:
;;   list of data for success answer
(defun body-list/success (config)
  (let
      ((message (config/get *message-field* config)))
    (list (cons "message" message))))

;; answers/get-success
;;
;;
;; Description:
;;   procedure for get success answer
;; Params:
;;   success         [List]      list of configs success answers
;;   content-type    [String]    content type
;;   key             [String]    name of success answer
;; Returns:
;;   list of answer
(defun answers/get-success (success content-type key)
  (let*
      ((property-config (config/get key success))
       (code (config/get *code-field* property-config))
       (body-list (body-list/success property-config)))
    (answer/get code content-type body-list)))
