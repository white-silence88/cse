;; routes.lisp
(in-package :cse)



;; check-method-on-allowed
;;
;;
;; Description:
;;   procedure check method on allowed
;; Params:
;;   method   [String]   name of method from request
;;   methods  [List]     list of simple array charaset (allowed method names)
;; Returns:
;;   result check. Procedure return True (T) when method allowed.
;;   Procedure return False (nil) when method not allowed
(defun check-method-on-allowed (method methods)
  (let
      ((to-check (string-downcase method)))
    (if (member to-check methods :test #'checks/compare-methods-names) t nil)))


;; conrollres/routes->founded
;;
;;
;; Description:
;; Params:
;; Returns:
(defun conrollres/routes->founded
    (content-type request route success redirection informational client-errors server-errors)
  (let*
      ((route-config (cdr route))
       (route-methods (gethash 'methods route-config))
       (request-method (string-downcase (config/get "method" request)))
       (on-request (gethash 'on-request route-config))
       (on-response (gethash 'on-response route-config))
       (handlers (gethash 'handlers route-config)))
    (if (not (check-method-on-allowed request-method route-methods))
        (progn
          (log:error "Method \"~a\" not allowed. Not correct request~%" request-method)
          (seon-answers/errors->method-not-allowed client-errors content-type))
        (let*
            ((result (if (not on-request)
                        (cons request nil)
                        (middlewares/loop on-request request nil)))
             (req (car result))
             (errs (cdr result)))
          (log:info "REQ: ~a~% " req)
          (log:info "ERRS: ~a~%" errs)
          (cond
            ((not errs)
             (progn
               (seon-answers/success success content-type *ok*)))
            (t (progn
                 (log:error "Errors: ~a~%" errs)
                 (seon-answers/success success content-type *ok*))))))))

;; controllers/routes->not-found
;;
;;
;; Description:
;;   procedure for get answer, when route not found
;; Params:
;;   client-errors  [List]    config list for client errors
;;   content-type   [String]  content type
;; Returns:
;;   nil
(defun controllers/routes->not-found (client-errors content-type)
  (seon-answers/errors->not-found client-errors content-type))
