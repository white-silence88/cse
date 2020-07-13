(in-package :cse)


;; reaction/route-not-found
;;
;;
;; Description:
;;   procedure for get answer, when route not found
;; Params:
;;   client-errors  [List]    config list for client errors
;;   content-type   [String]  content type
;; Returns:
;;   nil
(defun reaction/route-not-found (client-errors content-type)
  (seon-answers/errors->not-found client-errors content-type))

;; check-method-on-allowed
;;
;;
;; Description:
;;   procedure check method on allowed
;; Params:
;;   request-method   [String]   name of method from request
;;   route-methods    [List]     list of simple array charaset (allowed method names)
;; Returns:
;;   result check. Procedure return True (T) when method allowed.
;;   Procedure return False (nil) when method not allowed
(defun check-method-on-allowed (request-method route-methods)
  (let
      ((method-to-check (string-downcase request-method)))
    (if (member method-to-check route-methods :test (lambda (k v) (string= k (string v)))) t nil)))

;; reaction/route-is-found
;;
;;
;; Description:
;; Params:
;; Returns:
(defun reaction/route-is-found
    (content-type request route success redirection informational client-errors server-errors)
  (let*
      ((route-config (cdr route))
       (route-methods (gethash 'methods route-config))
       (request-method (string-downcase (config/get "method" request))))
    (if (not (check-method-on-allowed request-method route-methods))
        (progn
          (log:error "Method \"~a\" not allowed. Not correct request~%" request-method)
          (seon-answers/errors->method-not-allowed client-errors content-type))
        (seon-answers/success success content-type *ok*))))

;; http->run
;;
;;
;; Description:
;;   procedure for run HTTP server
;; Params:
;;   routes-config    [List]   routes config list
;;   answers-config   [List]   answers config list
;; Returns:
;;   nil
(defun http->run (routes-config answers-config)
  (let
      ((routes-map (routes-config->>routes-map routes-config))
       (success (config/get *default-success-field* answers-config))
       (redirection (config/get *default-redirection-field* answers-config))
       (informational (config/get *default-informational-field* answers-config))
       (client-errors (config/get *default-client-errors-field* answers-config))
       (server-errors (config/get *default-server-errors-field* answers-config)))
    (woo:run
     (lambda (env)
       (let*
           ((server-config nil)
            (request (woo/env->>request env))
            (request-url (config/get *default-baseurl-field* request))
            (route (routes-map/find routes-map request-url))
            (content-type *content-type-for-api*))
         (cond
          ((not route)
           (reaction/route-not-found client-errors content-type))
          (t
           (reaction/route-is-found
            content-type
            request
            route
            success
            redirection
            informational
            client-errors
            server-errors))))))))

;; application->start
;;
;;
;; Description:
;;   procedure for create thread with application
;; Params:
;;   name              [String]   name of service
;;   routs-config      [List]     routes config list
;;   answers-config    [List]     answers config list
;; Return:
;;   nil
(defun application->start(name routes-config answers-config)
  (cond
   ((string-equal name *application/http*)
    (progn
      (log:info "Starting thread with name \"~a\"...." name)
      (bt:make-thread (lambda () (http->run routes-config answers-config)) :name name)
      (log:info "Thread started. Find in threads...")
      (log:info "Thread info: ~a~%" (application->get/thread-by-name name))))
   (t (log:error "Not correct name (~a) or application type not allowed.~%" name))))


;; thread/check-name
;;
;;
;; Description:
;;   procedure for compare name and thread name
;; Params:
;;   name     [String]     name of thread for compare
;;   thread   [Thread]     thread (from bt-threads)
;; Returns:
;;   check result
(defun thread/check-name (name thread)
  (string= (bt:thread-name thread) name))

;; application->kill
;;
;;
;; Description:
;;   public procedure for kill thread by name
;; Params:
;;   name   [string]   name of thread to kill
;; Return:
;;   true or nil
(defun application->kill (name)
  (let*
      ((all-threads (bt:all-threads))
       (thread (find name all-threads :test #'thread/check-name)))
    (cond
      ((not thread)
       (progn
         (log:warn "Can not find thread with name \"~a\"." name)
         nil))
      (t
       (progn
         (bt:destroy-thread thread)
         (log:info "Success! Thread with name \"~a\" is killed." name)
         t)))))

;; application->info
;;
;;
;; Description:
;;   public procedure for get all threads for application server
;; Return:
;;   list of application threads
(defun application->info/threads ()
  (let*
      ((current-thread (bt:current-thread))
       (current-thread-name (bt:thread-name current-thread))
       (all-threads (bt:all-threads)))
    (progn
      (log:info "Current theread name: ~a.~%" current-thread-name)
      (log:info "All thread:~%~{~a~%~}~%" all-threads)
      all-threads)))


;; application->get/threads
;;
;;
;; Description:
;;   public procedure for get application threads
;; Params:
;;   nil
;; Return:
;;   list of threads
(defun application->get/threads ()
  (let
      ((all-threads (bt:all-threads)))
    (progn
      (log:info "All thread:~%~{~a~%~}~%" all-threads)
      all-threads)))


;; application->get/thread-by-name
;;
;;
;; Description:
;;   public procedure for get thread by name
;; Params:
;;   name    [string]    name of thread
;; Return:
;;   thread or nil
(defun application->get/thread-by-name (name)
  (let*
      ((all-threads (bt:all-threads))
       (thread (find name all-threads :test #'thread/check-name)))
    (progn
      (cond
        ((not thread) (log:warn "Can not find thread with name \"~a\".~%" name))
        (t (log:info "Thread with name \"~a\" was finded.~%" name)))
      thread)))
