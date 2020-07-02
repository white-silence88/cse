;; woo.lisp
(in-package #:cse)

;; request/request-uri
(defun request/request-uri (value)
  (cons *default-fullurl-field* value))

;; request/path-info
(defun request/path-info (value)
  (cons *default-baseurl-field* value))

;; get-method-pair
;;
;; Description:
;;   method for get pair with method name
;; Params:Ну как - 
;;   method-name  [String]  name of request method
;; Returns:
;;   pair with name property (method) and name
(defun get-method-pair (method-name)
  (cons *default-method-field* method-name))

;; request/method
;;
;; Description:
;;   procedure for get method proprty pair from env value
;; Params:
;;   value  [Symbol]  symbol of reqeust method
;; Returns
;;   pair with name property (method) and name
(defun request/method (value)
  (cond
   ((eq value :GET) (get-method-pair *http-method-get*))
   ((eq value :POST) (get-method-pair *http-method-post*))
   ((eq value :PUT) (get-method-pair *http-method-put*))
   ((eq value :DELETE) (get-method-pair *http-method-delete*))
   ((eq value :PATCH) (get-method-pair *http-method-pathc*))
   (t (get-method-pair *http-method-get*))))

;; create-or-update/list?
;;
;;
;; Description:
;;   procedure for check property on list type by name
;; Params:
;;   key  [String]  name of property
;; Returns:
;;   result check property on list type (T or nil)
(defun create-or-update/list? (key)
  (if (search *array-query-symbols* key) t nil))

;; find-param/test
;;
;;
;; Description:
;;   procedure for find element by property name
;; Params:
;;   key   [String]  name of property
;;   item  [Pair]    property pair
;; Returns:
;;   result check (T or nil)
(defun find-param/test (key item)
  (string= (car item) key))

;; update/create-or-update
;;
;;
;; Description:
;; Params:
;;   key     [String]  key for create/update property
;;   value   [String]  value of property
;;   result  [List]    list with pair query properties
;; Returns:
;;   list with query properies
(defun update/create-or-update (key value result)
  (let
      ((is-list (create-or-update/list? key))
       (existed-item (find key result :test #'find-param/test)))
    (cond
     ((not existed-item)
      (append result (list (cons key (if (not is-list) value (list value))))))
     (t
      (progn
        (setf (cdr existed-item) (append (cdr existed-item) (list value)))
        result)))))

;; element/update
;;
;;
;; Description:
;;   procedure for update list with query params
;; Params:
;;   element-list  [Pair]  param pair (key & value)
;;   result        [List]  list with query params
;; Results:
;;   updated list with query params
(defun element/update (element-list result)
  (let
      ((key (car element-list))
       (value (cdr element-list)))
    (cond
     ((not result) (list (cons key value)))
     (t (update/create-or-update key value result)))))

;; query-string/by-element
;;
;;
;; Descriprion:
;; Params:
;;   qlist   [List]  list with query params getted from query string
;;   result  [List]  list of query params
;; Returns:
;;   updated list query params
(defun query-string/by-element (qlist result)
  (let*
      ((first-element (first qlist))
       (rest-elements (rest qlist))
       (element-list (cl-ppcre:split *query-string-separator* first-element))
       (current-result (element/update element-list result)))
    (cond
     ((not rest-elements) current-result)
     (t (query-string/by-element rest-elements current-result)))))

;; request/query-string
;;
;;
;; Description:
;;   procedure for build query params from query string
;; Params:
;;   value  [String]  current value of query string
;; Returns:
;;   query params list
(defun request/query-string (value result)
  (let
      ((qlist (if (not value)
                  nil
                (cl-ppcre:split *query-params-separator* value))))
    (cons *default-query-params-field*
          (cond
           ((not qlist) (list result))
           (t (query-string/by-element qlist result))))))

;; request/headers
;;
;; Description:
;;   procedure for convert headers HashTable to config list
;; Params:
;;   headers  [HashTable]  headers hash table
;; Returns:
;;   pair with headers property
(defun request/headers (headers)
  (let
      ((keys (alexandria:hash-table-keys headers)))
    (cons *default-headers-field* (cond
                                   ((not headers) (list nil))
                                   (t (map 'list
                                           (lambda (key)
                                             (cons key (gethash key headers)))
                                           keys))))))

;; request/content-type
;;
;;
;; Description:
;;   procedure return content type
;; Params:
;;   value  [String]  some value content type property
;; Returns:
;;   config pair of content type property
(defun request/content-type (value)
  (cons *default-content-type-field* value))

;; request/unknown
;;
;;
;; Description:
;;   procedure return unknown property
;; Params:
;;   value  [String]  some value unknown property
;; Returns:
;;   config pair unknown property
(defun request/unknown (value)
  (cons *default-unknown-field* value))

;; woo-request->list
;;
;;
;; Description:
;;   procedure for get list request config from env
;; Params:
;;   env  [PropertyList]  request RAW property list data
;; Returns:
;;   request config list
(defun woo/env->>request (env)
  (let
      ((keys
        (list
         :REQUEST-URI
         :PATH-INFO
         :REQUEST-METHOD
         :QUERY-STRING
         :HEADERS
         :CONTENT-TYPE)))
    (map 'list
         (lambda (key)
           (let
               ((value (getf env key)))
             (cond
              ((eq key :REQUEST-URI) (request/request-uri value))
              ((eq key :PATH-INFO) (request/path-info value))
              ((eq key :REQUEST-METHOD) (request/method value))
              ((eq key :QUERY-STRING) (request/query-string value nil))
              ((eq key :HEADERS) (request/headers value))
              ((eq key :CONTENT-TYPE) (request/content-type value))
              (t (request/unknown value))))) keys)))
