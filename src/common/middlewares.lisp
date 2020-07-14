(in-package :cse)

;; middlewares/loop
;;
;;
;; Descriprion:
;; Params:
;; Returns:
(defun middlewares/loop (middlewares request errors)
  (let*
      ((middleware-name (string (first middlewares)))
       (rest-middlewares (rest middlewares))
       (middleware-symbol (if (not middleware-name) nil (find-symbol (string-upcase middleware-name))))
       (middleware-fn (if (not middleware-symbol) nil (symbol-function middleware-symbol)))
       (result (if (not middleware-fn)
                   (cons request errors)
                   (funcall middleware-fn request errors))))
    (cond
      ((not rest-middlewares) result)
      (t (middlewares/loop rest-middlewares (car result) (cdr result))))))
