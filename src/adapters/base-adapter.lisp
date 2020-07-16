(in-package :cse)
;; from-env/get-param->method
;;
;;
;; Description:
;;   procedure for get method config property
;; Params:
;;   name    [String]    name of method property
;;   value   [Symbol]    value of property
;; Returns:
;;   config pair for method property
(defun from-env/get-param->method (name value)
  (let
      ((method-value (cond
                       ((eq value :GET) *http-method-get*)
                       ((eq value :POST) *http-method-post*)
                       ((eq value :PUT) *http-method-put*)
                       ((eq value :DELETE) *http-method-delete*)
                       ((eq value :PATCH) *http-method-patch*)
                       (t *http-method-get*))))
    (from-env/get-param->simple name method-value)))

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
;; Description:
;;   procedure for get query param by iteration (by elements)
;; Params:
;;   query-list   [List]   list of query params
;;   result       [List]   result of last iterarion
;; Returns:
;;   list of query params
(defun query-string/by-element (query-list result)
  (let*
      ((first-element (first query-list))
       (rest-elements (rest query-list))
       (element-list (cl-ppcre:split *query-string-separator* first-element))
       (current-result (element/update element-list result)))
    (cond
      ((not rest-elements) current-result)
      (t (query-string/by-element rest-elements current-result)))))

;; from-env/get-param->query-string
;;
;;
;; Description:
;;   procedure for get config pair from query string 
;; Params:
;;   name    [String]   name of property
;;   value   [String]   value of property
;; Returns:
(defun from-env/get-params->query-string (name value)
  (let*
      ((qlist (if (not value) nil (cl-ppcre:split *query-params-separator* value)))
       (result (cond
                 ((not qlist) (list nil))
                 (query-string/by-element qlist nil))))
    (cons *default-query-params-field* result)))

;; from-env/get-param->hash-value
;;
;;
;; Description:
;;   procedure for get config pair from HashTable value
;; Params:
;;   name        [String]      name of config property
;;   hash-value  [HashTable]   value hash table
;; Returns:
;;   nil
(defun from-env/get-param->hash-value (name hash-value)
  (let
      ((keys (alexandria:hash-table-keys hash-value)))
    (from-env/get-param->simple name (cond
                                       ((not hash-value) (list nil))
                                       (t (map 'list
                                               (lambda (key)
                                                 (cons key (gethash hash-value key)))
                                               keys))))))

;; get-param/simple
;;
;;
;; Description:
;;   procedure for get simple request config pair
;; Params:
;;   name    [String]   name of property
;;   value   [Any]      value of property
;; Returns:
;;   config pair
(defun from-env/get-param->simple (name value)
  (cons name value))

;; env->request
;;
;;
;; Description:
;;   procedure for get request config from env request plist
;; Params:
;;   env      [List]       request env list
;;   configs  [List]       pairs with symbols and names for properies
;;   fn       [Function]   procedure for get config pair
;; Returns:
;;   list of configs
(defun env->request (env configs fn)
  (map 'list
       (lambda (config)
         (let*
             ((key (car config))
                              (name (cdr config))
              (value (getf env key)))
           (funcall fn key name value))) configs))

;; adapters/base
;;
;;
;; Description:
;;   base adapter procedure
;; Params:
;;   env              [List]      property list with request env
;;   request          [List]      list with request config
;;   success          [List]      list with success answer config
;;   redirection      [List]      list with redirection answer config
;;   informational    [List]      list with informational answer config
;;   client-errors    [List]      list with client errors answer config
;;   server-errors    [List]      list with server errors answer config
;;   req-fn           [Function]  procedure for get request config fromo request env
;;   reaction-fn      [Function]  procedure for get reactions by routes
;; Returns
;;   nil
(defun adapters/base
    (env routes-map success redirection informational client-errors server-errors req-fn reaction-fn)
  (let*
      ((server-config nil)
       (request (funcall req-fn env))
       (request-url (config/get *default-baseurl-field* request))
       (route (routes-map/find routes-map request-url))
       (content-type *content-type-for-api*))
    (funcall
     reaction-fn
     content-type route request success redirection informational client-errors server-errors)))
