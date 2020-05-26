(in-package #:cse)

;; pair/test-on-name
;;
;;
;; Description:
;;   test on name has pair config property
;; Params:
;; Returns:
;;   return t as string equal, or nil as not equal
(defun pair/test-on-name (value item)
  (string= value (car item)))


;; group?
;;
;;
;; Description:
;;   procedure for check on group config tree
;; Params:
;;   data   [List|Pair]   pair of data
;; Returns:
(defun group? (data)
  (let
      ((result (find "routes" data :test #'pair/test-on-name)))
    (cond
      ((not result) nil)
      (t t))))


;; config-property->get
;;
;;
;; Description:
;;   prcedure for get property value from config pair
;; Params:
;; Returns:
(defun config-property->get (tree-body property-name)
  (cdr (find property-name tree-body :test #'pair/test-on-name)))


;; middlewares/add
;;
;;
;; Description:
;;   procedure for add middlewares for list
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
;; Returns:
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
;; Returns:
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
;; Returns:
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
;; Params:
;; Returns:
(defun parser/update-path-or-prefix (group-prefix tname)
  (cond
   ((not group-prefix) tname)
   (t (concatenate 'string tname group-prefix))))


;; parser/add-route-pair
;;
;;
;; Description:
;; Params:
;; Returns:
(defun parser/add-pair
    (tname tbody desc cur-req-mid cur-res-mid result group-prefix)
  (let*
      ((path (parser/update-path-or-prefix tname group-prefix))
       (handler (config-property->get tbody "handler"))
       (methods (config-property->get tbody "methods"))
       (fields
        (route-config/make-config-list
         (list desc methods cur-req-mid cur-res-mid handler)
         (list 'description 'method 'on-request 'on-response 'hander)))
       (config (route-config/make fields))
       (pair-list (list (cons path config))))
    (cond
     ((not result) pair-list)
     (t (append result pair-list)))))


;; group-iteration/map
;;
;;
;; Description:
;; Params:
;; Returns:
(defun group-iteration/map (tree req-mid res-mid result prefix)
  (let*
      ((first-item (first tree))
       (rest-items (rest tree))
       (current (config/parser first-item req-mid res-mid result prefix)))
    (cond
     ((not rest-items) current)
     (t (group-iteration/map rest-items req-mid res-mid current prefix)))))


;; parse/group-iteration
;;
;;
;; Description:
;; Params:
;;   tname    [String]
;;   tbody    [List]
;;   req-mid  [List]
;;   res-mid  [List]
;;   result   [List]
;;   prefix   [Str]
;; Returns:
;;   list pair (url and config HashTable)
(defun parser/group-iteration (tname tbody req-mid res-mid result prefix)
  (let*
      ((new-prefix (parser/update-path-or-prefix prefix tname))
       (internal (config-property->get tbody "routes")))
    (group-iteration/map internal req-mid res-mid result new-prefix)))

;; config/parser
;;
;;
;; Description:
;;   parser for routes tree config
;; Params:
;;  tree            [List] list of config pairs
;;  req-middlewares [List] list of functions for hook before require
;;  res-middlewares [List] list of function for hook before response
;;  result          [List] list of pair (url and config HashTables)
prefix
;; Returns:
;;   list of pair (url and config HashTable)
(defun config/parser (tree req-middlewares res-middlewares result prefix)
  (let*
      ((tname (car tree))
       (tbody (cdr tree))
       (is-group (group? tbody))
       (cur-req-mid (middlewares/add tbody "on-request" req-middlewares))
       (cur-res-mid (middlewares/add tbody "on-request" res-middlewares))
       (desc (config-property->get tbody "description")))
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
