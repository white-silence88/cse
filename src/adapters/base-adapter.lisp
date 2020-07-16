(in-package :cse)
;; from-env/get-param->method
;;
;;
;; Description:
;; Params:
;; Returns:
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
;; Params:
;; Returns:
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
;; Params:
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
;; Params:
;; Returns:
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
;; Params:
;; Returns:
(defun from-env/get-param->simple (name value)
  (cons name value))

;; env->request
;;
;;
;; Description
;;   common adapter function
;; Params
;; Returns
(defun env->request (env configs fn)
  (map 'list
       (lambda (config)
         (let*
             ((key (car config))
              (name (cdr config))
              (value (getf env key)))
           (funcall fn key name value)))
       configs))
