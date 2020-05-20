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


;; config/parser
;;
;;
;; Description:
;;   parser for routes tree config
(defun config/parser (tree req-middlewares res-middlewares)
  (let*
      ((config-name (car tree))
       (tree-body (cdr tree))
       (is-group (group? tree-body))
       (current-req-middlewares (middlewares/add tree-body "on-request" req-middlewares))
       (current-res-middlewares (middlewares/add tree-body "on-request" res-middlewares)))
    (list tree-name is-group current-req-middlewares current-res-middlewares)))

;; wood->>routes
;;
;;
;; Description:
;;   public procedure convert routes tree config to routes map
(defun routes-config->>routes-map (routes-wood)
  (map 'list (lambda (routes-tree) (tree/parser routes-tree nil nil)) routes-wood))
