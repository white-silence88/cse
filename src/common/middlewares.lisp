(in-package :cse)

;; middlewares/loop
;;
;;
;; Descriprion:
;; Params:
;; Returns:
(defun middlewares/loop (middlewares request response errors)
  (let*
      ((middleware-name (string (first middlewares)))
       (rest-middlewares (rest middlewares))
       (middleware-symbol (cond
                            ((not middleware-name) nil)
                            (t (find-symbol (string-upcase middleware-name)))))
       (middleware-fn (cond
                        ((not middleware-symbol) nil)
                        (t (symbol-function middleware-symbol))))
       (result (cond
                 ((not middleware-fn)
                  (list (cons "request" request) (cons "response" response) (cons "errors" errors)))
                 (t (funcall middleware-fn request response errors)))))
    (cond
      ((not rest-middlewares) result)
      (t (middlewares/loop rest-middlewares
                           (config/get "request" result)
                           (config/get "response" result)
                           (config/get "errors" result))))))
