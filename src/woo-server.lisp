(in-package #:cse)

;; request/request-uri
(defun request/request-uri (value)
  (cons "full-url" value))

;; request/path-info
(defun request/path-info (value)
  (cons "base-url" value))

(defun get-method-pair (method-name)
  (cons "method" method-name))

;; request/method
(defun request/method (value)
  (cond
   ((eq value :GET) (get-method-pair "GET"))
   ((eq value :POST) (get-method-pair "POST"))
   ((eq value :PUT) (get-method-pair "PUT"))
   ((eq value :DELETE) (get-method-pair "DELETE"))
   ((eq value :PATCH) (get-method-pair "PATCH"))
   (t (get-method-pair "GET"))))

(defun cou/list? (key)
  (if (search "[]" key) t nil))

(defun cou/test (to-find item)
    (string= (car item) to-find))

(defun update/cou (key value result)
  (let
      ((is-list (create-or-update/list? key))
       (existed-item (find key result :test #'cou/test)))
    (if (not existed-item)
        (let
            ((new-elem (list (cons key (if (not is-list) value (list valie))))))
          (append result new-elem))
      (let
          ((vexist (cdr existed-item)))
        (progn
          (setf vexist (if (not is-list) value (append vexist value)))
          result)))))

(defun query-element/update (elem-pair result)
  (let
      ((key (car elem-pair))
       (value (cdr elem-pair)))
    (if (not result)
        (list (cons key value))
      (update/cou key value result))))


(defun query-string/query-element (qlist result)
  (let
      ((felem-qlist (first qlist))
       (relem-qlist (rest qlist))
       (elem-pair (cl-ppcre:split "="))
       (current-result (query-element/update elem-pair result)))
    (if (not relem-qlist) current-result
      (query-string/query-element relem-qplist result))))

;; request/query-string
(defun request/query-string (value)
  (let*
      ((qlist (if (not value) nil (cl-ppcre:split "&" value)))
       (qplist (if (not qlist) nil (query-string/query-element qlist))))
    ()))

;; request/headers
(defun request/headers (headers)
  (if (not headers)
      nil
    (let
        ((headers-keys (alexandria:hash-table-keys headers)))
      (map 'list
           (lambda (header-key)
             (cons header-key (gethash header-key headers))) headers-keys))))

;; request/content-type
;;
;;
;; Description:
;;   procedure return content type
;; Returns:
;;   
(defun request/content-type (value)
  (cons "content-type" value))

;; request/unknown
;;
;;
;; Description:
;;   procedure return unknown property
(defun requrest/unknown (value)
  (cons "unknown" value))

;; woo-request->list
;;
;;
;; Description:
;;   procedure for get list request config from env
(defun woo-request->list (env)
  (let
      ((req-keys
        (list
         :REQUEST-URI
         :PATH-INFO
         :METHOD
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
              ((eq key :METHOD) (request/method value))
              ((eq key :QUERY-STRING) (request/query-string value nil))
              ((eq key :HEADERS) (request/headers value))
              ((eq key :CONTENT-TYPE) (request/content-type value))
              (t (request/unknown value)))))
         req-keys)))
