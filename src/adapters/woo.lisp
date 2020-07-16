;; woo.lisp
(in-package #:cse)

;; Define request method symbol for Woo
(defparameter *woo-request-method-symbol* :REQUEST-METHOD)
;; Define query string symbol for Woo
(defparameter *woo-query-string-symbol* :QUERY-STRING)
;; Define path info symbol for Woo
(defparameter *woo-path-info-symbol* :PATH-INFO)
;; Define request URI symbol for Woo
(defparameter *woo-request-uri-symbol* :REQUEST-URI)
;; Define headers symbol for Woo
(defparameter *woo-headers-symbol* :HEADERS)
;; Define content type symbol for Woo
(defparameter *woo-content-type-symbol* :CONTENT-TYPE)

;; woo/check
;;
;;
;; Description:
;; Params:
;; Returns:
(defun woo/check (key name value)
  (cond
    ((eq key *woo-request-method-symbol*) (from-env/get-param->method name value))
    ((eq key *woo-query-string-symbol*) (from-env/get-params->query-string name value))
    ((eq key *woo-headers-symbol*) (from-env/get-param->hash-value name value))
    (t (from-env/get-param->simple name value))))

;; woo-request->list
;;
;;
;; Description:
;;   procedure for get list request config from env
;; Params:
;;   env  [PropertyList]  request RAW property list data
;; Returns:
;;   request config list
(defun woo->>request (env)
  (let
      ((configs (list
                 (cons *woo-request-uri-symbol* *default-fullurl-field*)
                 (cons *woo-path-info-symbol* *default-baseurl-field*)
                 (cons *woo-request-method-symbol* *default-method-field*)
                 (cons *woo-query-string-symbol* *default-query-params-field*)
                 (cons *woo-query-string-symbol* *default-headers-field*)
                 (cons *woo-content-type-symbol* *default-content-type-field*))))
    (env->request env configs #'woo/check)))
