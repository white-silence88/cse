;; values.lisp
;; File include all paramaters and constants used in Chainsaw Service Engine
(in-package #:cse)
;;
;; ============================================================================
;;                            ABSTRACT PARAMETERS
;; ============================================================================
;;
;; Define minimal lengt for lists and strings
(defparameter *min-lengt* 0)
;; Define start of name index in URL token
(defparameter *start-name-index* 1)
;; Define first index number for lists and strings
(defparameter *zero-index* 0)

;;
;; ============================================================================
;;                            DEFAULT FILENAMES
;; ============================================================================
;;
;; Define default name for server config file
(defparameter *default-server-config-name* "config.json")
;; Define default name for routes config file
(defparameter *default-routes-config-name* "routes.json")
;; Define default name for success config file
(defparameter *default-success-config-name* "success.json")
;; Define default name for client errors file
(defparameter *default-client-errors-config-name* "client-errors.json")
;; Define default name for server errors file
(defparameter *default-server-erorrs-config-name* "server-errors.json")
;; Define default name for informational answers file
(defparameter *default-informational-config-name* "informational.json")
;; Define default name for redirection answer field
(defparameter *default-redirection-config-name* "redirection.json")


;;
;; ============================================================================
;;                            ANSWER CONFIG FIELDS
;; ============================================================================
;;
;; Default field name of success property
(defparameter *default-success-field* "success")
;; Default field name of redirection property
(defparameter *default-redirection-field* "redirection")
;; Default field name of informational property
(defparameter *default-informational-field* "informational")
;; Default field name of client errors property
(defparameter *default-client-errors-field* "client-errors")
;; Default field name of server errors property
(defparameter *default-server-errors-field* "server-errors")

;;
;; ============================================================================
;;                            REQUEST FIELDS
;; ============================================================================
;;
(defparameter *default-baseurl-field* "base-url")
(defparameter *default-fullurl-field* "full-url")
(defparameter *default-method-field* "method")
(defparameter *default-unknown-field* "unknown")
(defparameter *default-headers-field* "headers")
(defparameter *default-content-type-field* "content-type")
(defparameter *default-query-params-field* "query-params")
;;
;; ============================================================================
;;                           HTTP METHOD NAMES
;; ============================================================================
;;
(defparameter *http-method-get* "GET")
(defparameter *http-method-post* "POST")
(defparameter *http-method-put* "PUT")
(defparameter *http-method-delete* "DELETE")
(defparameter *http-method-patch* "PATCH")
(defparameter *http-method-options* "OPTIONS")
;;
;; ============================================================================
;;                           HTTP QUERY PARAMS
;; ============================================================================
;;
(defparameter *array-query-symbols* "[]")
(defparameter *query-string-separator* "=")
(defparameter *query-params-separator* "&")
(defparameter *url-separator* "/")
(defparameter *param-symbol* ":")

;;
;; ============================================================================
;;                           CONFIG FIELDS
;; ============================================================================
;;
(defparameter *default-server-field* "server")
(defparameter *default-routes-field* "routes")
;;
;; ============================================================================
;;                      HEADERS (NAMES AND VALUES)
;; ============================================================================
;;
(defparameter *content-type-for-api* "application/json")

;;
;; ============================================================================
;;                           ANSWERS CONFIG FIELDS
;; ============================================================================
;;
(defparameter *code-field* "code")
(defparameter *message-field* "message")
;;
;; ============================================================================
;;                           WORK WITH JSON FILE
;; ============================================================================
;;
(defparameter *simple-tree* "common")
(defparameter *routes-tree* "routes")
;;
;; ============================================================================
;;                           ROUTES CONFIG FIELDs
;; ============================================================================
;;
(defparameter *route-config-routes-field* "routes")
(defparameter *route-config-description-field* "description")
(defparameter *route-config-on-request-field* "on-request")
(defparameter *route-config-on-response-field* "on-response")
(defparameter *route-config-methods-field* "methods")
(defparameter *route-config-handler-field* "handler")

;;
;; ============================================================================
;;                           SUCCESS ANSWERS NAMES
;; ============================================================================
;;

(defparameter *ok* "ok")
;;; Define parameter name of not found key
(defparameter *not-found-config-key* "notFound")
;;; Define parameter name of not allowed method
(defparameter *method-not-allowed-config-key* "methodNotAllowed")

;;
;; ============================================================================
;;                             APPLICATION NAMES
;; ============================================================================
;;
(defparameter *application/http* "http")
