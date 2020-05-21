(in-package #:cse)

;; pair/test-on-name
;;
;;
;; Description:
;;   test on name pair config property
(defun pair/test-on-name (value item)
  (string= value (car item)))

;; group?
;;
;; Description:
;;   procedure for check on group config tree
(defun group? (tree-body)
  (let
      ((result (find "routes" tree-body :test #'pair/test-on-name)))
    (cond
      ((not result) nil)
      (t t))))

;; config-property->get
;;
;; Description:
;;   prcedure for get property value from config pair
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
(defun route-config/make (config-list)
  (let
      ((config (make-hash-table)))
    (route-config/make->update-fields config config-list)))

;; routes-config/make-config-list
;;
;;
;; Description:
;;   procedure for make config as pair list from two arays
(defun route-config/make-config-list (values-list fields-list)
  (let
      ((index -1))
    (map 'list
         (lambda (value)
           (progn
             (setq index (+ index 1))
             (cons (nth index fields-list) value))) values-list)))

;; parser/update-path-or-prefix
(defun parser/update-path-or-prefix (group-prefix tname)
  (cond
   ((not group-prefix) tname)
   (t (concatenate 'string group-prefix tname))))

;; parser/add-route-pair
(defun parser/add-route-pair
    (tname tbody desc cur-req-mid cur-res-mid result group-prefix)
  (let*
      ((path (parser/update-path-or-prefix group-prefix))
       (handler (config-property->get tbody "handler"))
       (methods (confif-property->get tbody "methods"))
       (fields
        (route-config/make-config-list
         (list desc methods cur-req-mid cur-res-mid handler)
         (list 'description 'method 'on-request 'on-response 'hander)))
       (config (route-config/make fields)))
    (append result (list (cons path config)))))

;; parse/group-iteration
(defun parse/group-iteration
    (tname tbody cur-req-mid cur-res-mid result group-prefix)
  (let*
      ((new-group-prefix (parser/update-path-or-prefix group-prefix tname))
       (internal-wood (config-property->get tbody "routes"))
       (first-tree (first internal-wood))
       (rest-wood (rest internal-wood)))
    ;; TODO: add recurrsive iteration (or loop)
    ( )))

;; config/parser
;;
;;
;; Description:
;;   parser for routes tree config
(defun config/parser (tree req-middlewares res-middlewares result group-prefix)
  (let*
      ((tname (car tree))
       (tbody (cdr tree))
       (is-group (group? tbody))
       (cur-req-mid (middlewares/add tbody "on-request" req-middlewares))
       (cur-res-mid (middlewares/add tbody "on-request" res-middlewares))
       (desc (config-property->get tbody "description")))
    (cond
     ((not is-group)
      (parser/add-route-pair
       tname tbody desc cur-req-mid cur-res-mid result group-prefix))
     (t
      (parse/group-iteration
       tname tbody cur-req-mid cur-res-mid result group-prefix)))))

;; wood->>routes
;;
;;
;; Description:
;;   public procedure convert routes tree config to routes map
(defun routes-config->>routes-map (routes-wood)
  (map 'list
       (lambda (routes-tree)
         (config/parser routes-tree nil nil nil nil)) routes-wood))
