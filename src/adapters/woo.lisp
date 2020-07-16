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
;;   propcedure for get request config from woo env (by ckeck in base adapters)
;; Params:
;;   key     [Symbol]    property symbol from request plist
;;   name    [String]    name of property in request config
;;   value   [Any]       value of request property
;; Returns:
;;   pair config
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

;; adapters/woo
;;
;;
;; Description:
;;   adapter for Woo server
;; Params:
;;   routes-map       [List]      list of routes
;;   success          [List]      list with success answer config
;;   redirection      [List]      list with redirection answer config
;;   informational    [List]      list with informational answer config
;;   client-errors    [List]      list with client errors answer config
;;   server-errors    [List]      list with server errors answer config
;;   reaction-fn      [Function]  procedure for get reactions by routes
;; Returns:
(defun adapters/woo
    (routes-map success redirection informational client-errors server-errors reaction-fn)
  (woo:run
   (lambda (env)
     (adapters/base
      env
      routes-map success redirection informational client-errors server-errors #'woo->>request reaction-fn))))
