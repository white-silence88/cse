;; json-file
(in-package #:cse)
;; tree/build-key
;;
;;
;; Description:
;;   procedure for get value for key for tree pair
;; Params:
;;   key      [String] name of tree node
;;   type     [String] type of tree
;;   reserved [List<String>] list of reserved words
;; Returns
;;   name of treee node
(defun tree<-get-key (key type reserved)
  (cond
    ((member key reserved :test (lambda (k v) (string= k v))) key)
    (t (if (string= type "routes")
           (concatenate 'string "/" key)
           key))))

;; tree/value
;;
;;
;; Description:
;;   procedure for get value for current iterations.
;;   If type of value is List - add list procedure and return.
;;   If type of value is HashTable - run hash->>tree procedure with new current arguments
;;   Else - return value.
;; Params:
;;   value  [HashTable|List|Any] value for current key
;; Returns:
(defun tree<-get-value (value type)
  (cond
   ((typep value 'hash-table)
    (hash->>tree (alexandria:hash-table-keys value) value type))
   (t value)))

;; hash->>tree/iteration
;;
;;
;; Description:
;;   procedure for get result iteration content convertations
;; Params:
;;   key     [String] name of property
;;   content [HashTable<String|HashTable|List<String>>] routes HashTable
;;   type    [String] type of tree
;; Returns:
;;   current iteration pair (key and value)
(defun hash->>tree/iteration (key content type)
  (let
      ((value (gethash key content))
       (reserved
        (list
         *route-config-routes-field*
         *route-config-description-field*
         *route-config-on-request-field*
         *route-config-on-response-field*
         *route-config-methods-field*
         *route-config-handler-field*
         *route-config-answer-field*
         "title"
         "message"
         "description"
         "data")))
    (cons (tree<-get-key key type reserved) (tree<-get-value value type))))

;; hash->>tree
;;
;;
;; Description:
;;   procedure for create list of pairs from HashTable by keys
;; Params:
;;   keys [List<String>] list of hash table keys
;;   content [HashTable<String|HashTable|List<String>>] routes HashTable
;;   type     [String] type of tree
;; Returns:
;;   routes tree
(defun hash->>tree (keys content type)
  (map 'list
       (lambda (key)
         (hash->>tree/iteration key content type))
       keys))


;; file->hash->tree
;;
;;
;; Description:
;;   function for create routes tree from JSON file
;; Params:
;;   filepath [String] full file path
;;   type     [String] type of tree
;; Returns:
;;   routes file tree
(defun file->hash->tree (filepath type)
  (let*
      ((content-as-string (uiop:read-file-string filepath))
       (content-as-hash (jonathan:parse content-as-string :as :hash-table))
       (keys (alexandria:hash-table-keys content-as-hash)))
    (hash->>tree keys content-as-hash type)))

;; file->>routes-tree
;;
;;
;; Description:
;;   procedure for convert file content (.json-file) for route tree
;; Params:
;;   filepath [String] full file path
;; Returns:
;;   routes tree from JSON file
(defun json-file->>routes-tree (filepath)
  (file->hash->tree filepath *routes-tree*))

;; file->>tree
;;
;;
;; Description:
;;   procedure for convert file content (.json-file) for tree
;; Params:
;;   filepath [String] full file path
;; Returns:
;;   tree from JSON file
(defun json-file->>tree (filepath)
  (file->hash->tree filepath *simple-tree*))

