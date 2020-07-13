(in-package #:cse)
;; pair/test-on-name
;;
;;
;; Description:
;;   procedure for check on name pair config
;; Params:
;;   value   [String]   name of property
;;   item    [Pair]     config property pair
;; Returns:
;;   As values equal return t.
;;   As values not equal return nil
(defun pair/test-on-name (value item)
  (string= value (car item)))


;; group?
;;
;;
;; Description:
;;   procedure for check on group config tree. In procedure try find
;;   routes property.
;;   As property with name "routes" find - return t, else return nil
;; Params:
;;   data   [List|Pair]   pair of data
;; Returns:
;;   t as compare result is True.
;;   nil as compare result is False.
(defun group? (data)
  (let
      ((result (find *route-config-routes-field* data :test #'pair/test-on-name)))
    (cond
      ((not result) nil)
      (t t))))


;; config-property->get
;;
;;
;; Description:
;;   prcedure for get property value from config pair
;; Params:
;;   tree-body     [List] list of properties
;;   property-name [String] name of finded property
;; Returns:
;;   value of property or nil
(defun config-property->get (tree-body property-name)
  (cdr (find property-name tree-body :test #'pair/test-on-name)))


;; middlewares/add
;;
;;
;; Description:
;;   procedure for add middlewares for list
;; Params:
;;   tree-body      [List]   list with config pairs
;;   property-name  [String] name of property with middlewares
;;   middlewares    [List]   list of middlewares functions
;; Returns:
;;   new list of middlewares functions
(defun middlewares/add (tree-body property-name middlewares)
  (let
      ((property (config-property->get tree-body property-name)))
    (if (not property)
        middlewares
        (let
            ((result (list property)))
          (cond
            ((not middlewares) result)
            (t (append middlewares result)))))))


;; route-config/make->update-fields
;;
;;
;; Description:
;;   reccursive procedure for add fields for HashTable by list of pairs
;; Params:
;;   config      [HashTable]   route config HashTable
;;   config-list [List]        list of config pairs
;; Returns:
;;   updated config HashTable
(defun route-config/make->update-fields (config config-list)
  (let*
      ((field (first config-list))
       (rest-fields (rest config-list))
       (field-name (car field))
       (field-value (cdr field)))
    (progn
      (setf (gethash field-name config) field-value)
      (cond
       ((not rest-fields) config)
       (t (route-config/make->update-fields config rest-fields))))))


;; route-config/make
;;
;;
;; Description:
;;   procedure for make HashTable for routes config
;; Params:
;;   config-list [List] list of config pairs
;; Returns:
;;   HashTable with route config
(defun route-config/make (config-list)
  (let
      ((config (make-hash-table)))
    (route-config/make->update-fields config config-list)))


;; routes-config/make-config-list
;;
;;
;; Description:
;;   procedure for make config as pair list from two arays
;; Params:
;;   values-list  [List]  list of config values
;;   fields-list  [List]  list of config fields
;; Returns:
;;   list of route pairs (field and value)
(defun route-config/make-config-list (values-list fields-list)
  (let
      ((index -1))
    (map 'list
         (lambda (value)
           (progn
             (setq index (+ index 1))
             (cons (nth index fields-list) value))) values-list)))


;; parser/update-path-or-prefix
;;
;;
;; Description:
;;   procedure for update path or path prefix
;; Params:
;;   prefix   [String]   prefix for update path
;;   tname    [String]   path for update by prefix
;; Returns:
;;   new updated prefix
(defun parser/update-path-or-prefix (prefix tname)
  (cond
   ((not prefix) tname)
   (t (concatenate 'string tname prefix))))


;; parser/add-route-pair
;;
;;
;; Description:
;;   procedure for add pair to route map
;; Params:
;;   tname       [String]   current route path
;;   tbody       [List]     list of route config pairs
;;   desc        [String]   route description
;;   req-mid     [List]     list of requst middlewares
;;   res-mid     [List]     list of response middlewares
;;   result      [List]     list of pair (URL route & config for it)
;;   prefix      [String]   prefix for URL
;; Returns:
;;   list of pair (URL route & config for it)
(defun parser/add-pair
    (tname tbody desc req-mid res-mid result prefix)
  (let*
      ((path (parser/update-path-or-prefix tname prefix))
       (handler (config-property->get tbody *route-config-handler-field*))
       (methods (config-property->get tbody *route-config-methods-field*))
       (values (list desc methods req-mid res-mid handler))
       (keys (list 'description 'methods 'on-request 'on-response 'handlers))
       (fields (route-config/make-config-list values keys))
       (config (route-config/make fields))
       (pair-list (list (cons path config))))
    (cond
     ((not result) pair-list)
     (t (append result pair-list)))))


;; group-iteration/map
;;
;;
;; Description:
;;   recurrsive procedure for execute data list with pairs routes config &
;;   rebuild result array
;; Params:
;;   data      [List]    data of route tree config
;;   req-mid   [List]    list of request middlewares
;;   res-mid   [List]    list of response middlewares
;;   result    [List]    result list
;;   prefix    [String]  URL path prefix
;; Returns:
;;   list of pairs with route URL and route config HashTable
(defun group-iteration/map (data req-mid res-mid result prefix)
  (let*
      ((first-item (first data))
       (rest-items (rest data))
       (current (config/parser first-item req-mid res-mid result prefix)))
    (cond
     ((not rest-items) current)
     (t (group-iteration/map rest-items req-mid res-mid current prefix)))))


;; parse/group-iteration
;;
;;
;; Description:
;;   rescurrsive procedure for execute route list of config pairs
;; Params:
;;   tname    [String]  URL path
;;   tbody    [List]    list of config pairs
;;   req-mid  [List]    list of request middlewares
;;   res-mid  [List]    list of response middlewares
;;   result   [List]    result list
;;   prefix   [String]  URL path prefix
;; Returns:
;;   list pair (url and config HashTable)
(defun parser/group-iteration (tname tbody req-mid res-mid result prefix)
  (let*
      ((new-prefix (parser/update-path-or-prefix prefix tname))
       (internal (config-property->get tbody *route-config-routes-field*)))
    (group-iteration/map internal req-mid res-mid result new-prefix)))

;; config/parser
;;
;;
;; Description:
;;   parser for routes tree config
;; Params:
;;  tree               [List]    list of config pairs
;;  req-middlewares    [List]    list of functions for hook before require
;;  res-middlewares    [List]    list of function for hook before response
;;  result             [List]    list of pair (url and config HashTables)
;;  prefix             [String]  prefix part for current URL
;; Returns:
;;   list of pair (url and config HashTable)
(defun config/parser (tree req-middlewares res-middlewares result prefix)
  (let*
      ((tname (car tree))
       (tbody (cdr tree))
       (is-group (group? tbody))
       (cur-req-mid
        (middlewares/add tbody *route-config-on-request-field* req-middlewares))
       (cur-res-mid
        (middlewares/add tbody *route-config-on-response-field* res-middlewares))
       (desc (config-property->get tbody *route-config-description-field*)))
    (cond
     ((not is-group)
      (parser/add-pair
       tname tbody desc cur-req-mid cur-res-mid result prefix))
     (t
      (parser/group-iteration
       tname tbody cur-req-mid cur-res-mid result prefix)))))

;; routes-map/iteration
;;
;;
;; Description:
;;   procedure for execute iteration of routes config list
;; Params:
;;   data   [List] list of routes config pairs
;;   result [List] list of pair (url and config HashTable)
;; Returns:
;;   list of pair (url and config HashTable)
(defun routes-map/iteration (data result)
  (let*
      ((first-data (first data))
       (rest-data (rest data))
       (current (config/parser first-data nil nil result nil)))
    (cond
     ((not rest-data) current)
     (t (routes-map/iteration rest-data current)))))


;; routes-config->>routes-map
;;
;;
;; Description:
;;   public procedure convert routes tree config to routes map
;; Params:
;;   data   [List]  list of routes config pairs
;; Returns:
;;   list of pair (url and config HashTable)
(defun routes-config->>routes-map (data)
  (routes-map/iteration data nil))

;; url-param?
;;
;; Description:
;;   procedure for check route url token on param
;; Params:
;;   route-token   [String]   url token
;; Returns:
;;   result check on url param token (True - t, False - nil)
(defun is-url-param? (route-token)
  (string= *param-symbol* (subseq route-token *zero-index* *start-name-index*)))

;; params/update
;;
;;
;; Description:
;;   procedure for update url params alist
;; Params:
;;   is-param   [Boolean|nil]  property is equal t when token is param
;;   params     [List]         url params list
;;   name       [String]       name of url param
;;   value      [Value]        value of url param
;; Returns:
;;   list of url params
(defun params/update (is-param params name value)
  (if (eq is-param t)
      (let
          ((palist (list (cons (subseq name *start-name-index*) value))))
        (cond
         ((not params) palist)
         (t (append params palist))))
      params))

;; compare-lists/compare-tokens
;;
;;
;; Description:
;; Params:
;; Returns
(defun compare-tokens (is-param request-url-token route-url-token)
  (if (not is-param)
      (string= request-url-token route-url-token)
      t))

;; route-test/check-tokens
;;
;;
;; Description:
;;   procedure for compare url list and route list
;; Params:
;;   request-url-tokens   [List]        list with string request url tokens
;;   route-url-tokens     [List]        list with string route url tokens
;;   route-config         [HashTable]   HashTable with routw config
;;   params               [List|nil]    alist with route url params
;; Returns:
;;   return result check True (t) or False (nil)
(defun compare-lists (request-url-tokens route-url-tokens route-config params)
  (let*
      ((first-request-url-token (first request-url-tokens))
       (rest-request-url-tokens (rest request-url-tokens))
       (first-route-url-token (first route-url-tokens))
       (rest-route-url-tokens (rest route-url-tokens))
       (is-param (is-url-param? first-route-url-token))
       (compare-result (compare-tokens is-param first-route-url-token first-request-url-token))
       (new-params (params/update is-param params first-route-url-token first-request-url-token)))
    (cond
      ((not compare-result) nil)
      (t (if (not rest-request-url-tokens)
             (progn
               (setf (gethash 'url-params route-config) new-params)
               t)
             (compare-lists
              rest-request-url-tokens rest-route-url-tokens route-config new-params))))))

;; check-tokens/length
;;
;; Description:
;;   procedure for check token on zero length (for remove-if macros)
;; Params:
;;   token   [String]  element (URL token) for check
;; Returns:
;;   result of check (compare length and zero-length)
(defun check-tokens/length (token)
  (if (= (length token) *min-lengt*) t nil))

;; url->list
;;
;;
;; Description:
;;   procedure for convert url string to url list
;; Params:
;;   url   [String]   request URL
;; Returns:
;;   list with url string tokens
(defun url->list (url)
  (let
      ((raw (cl-ppcre:split *url-separator* url)))
    (remove-if #'check-tokens/length raw)))

;; route-test/not-equals
;;
;;
;; Description:
;;   procedure for run test for routes with not equal urls
;; Params:
;;   request-url    [String]      URL from request
;;   route-url      [String]      URL pattern from route settings
;;   route-config   [HashTable]   HashTable with route config
;; Returns:
;;   result check. True (t) if pattern is corrent, or False (nil) if pattern not equal
;;   requst url.
(defun route-test/not-equals (request-url route-url route-config)
  (let
      ((request-url-tokens (url->list request-url))
       (route-url-tokens (url->list route-url)))
    (cond
      ((= (list-length request-url-tokens) (list-length route-url-tokens))
       (compare-lists request-url-tokens route-url-tokens route-config nil))
      (t nil))))

;; find/route-test
;;
;;
;; Description:
;;   procedure for check route and url equals
;; Params:
;;   request-url  [String]  request URL
;;   route-pair   [Pair]    pair with url-pattern (string) and config (HashTable)
;; Returns:
;;   test result as True (t) or nil
(defun find/route-test (request-url route-pair)
  (let
      ((route-url (car route-pair))
       (route-config (cdr route-pair)))
    (cond
      ((string= request-url route-url) t)
      (t (route-test/not-equals request-url route-url route-config)))))

;; routes-map/find
;;
;;
;; Description:
;;   procedure for find route config in route map
;; Params:
;;   routes-map  [List]    alist with routes
;;   url         [String]  request url
;; Returns:
;;   pair of route pattern and route config
(defun routes-map/find (routes-map url)
  (find url routes-map :test #'find/route-test))
