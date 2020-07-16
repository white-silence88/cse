(in-package :cse)

;; http->base-routing
;;
;;
;; Description:
;;   procedure with base routing reactions
;; Params:
;;   content-type     [String]                      type of content
;;   route            [Pair<String, HashTable>]     route pattern and config
;;   request          [List]                        list with request config
;;   success          [List]                        list with success answer config
;;   redirection      [List]                        list with redirection answer config
;;   informational    [List]                        list with informational answer config
;;   client-errors    [List]                        list with client errors answer config
;;   server-errors    [List]                        list with server errors answer config
;; Returns:
;;   http answer in list type
(defun http->base-routing
    (content-type route request success redirection informational client-errors server-errors)
  (cond
    ((not route)
     (controllers/routes->not-found client-errors content-type))
    (t
     (conrollres/routes->founded
      content-type
      request
      route
      success
      redirection
      informational
      client-errors
      server-errors))))

;; http->run
;;
;;
;; Description:
;;   procedure for run HTTP server
;; Params:
;;   routes-config    [List]      routes config list
;;   answers-config   [List]      answers config list
;;   adapter          [Function]  adapter for HTTP application
;; Returns:
;;   nil
(defun http->run (routes-config answers-config adapter)
  (let
      ((routes-map (routes-config->>routes-map routes-config))
       (success (config/get *default-success-field* answers-config))
       (redirection (config/get *default-redirection-field* answers-config))
       (informational (config/get *default-informational-field* answers-config))
       (client-errors (config/get *default-client-errors-field* answers-config))
       (server-errors (config/get *default-server-errors-field* answers-config)))
    (funcall
     adapter
     routes-map success redirection informational client-errors server-errors #'http->base-routing)))


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
      (bt:make-thread (lambda () (http->run routes-config answers-config #'adapters/woo)) :name name)
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
