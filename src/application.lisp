(in-package :cse)

;; http->run
;;
;;
;; Description:
;;   procedure for run HTTP server
;; Returns:
;;   nil
(defun http->run ()
  (woo:run
   (lambda (env)
     (let*
         ((server-config nil)
          (request (woo/env->>request env))
          (response (answer->>jsonify (list (cons "message" "Example"))))
          (content-type "application/json"))
       `(200 (:content-type ,content-type) (,response))))))

;; application->start
;;
;;
;; Description:
;;   procedure for create thread with application
;; Params:
;;   name [string] name of service
;; Return:
;;   nil
(defun application->start(name)
  (cond
    ((string-equal name "http") (bt:make-thread #'http->run :name name))
    (t (format t "Not correct name (~a) or application type not allowed" name))))


;; kill-thread->iteration
;;
;;
;; Description:
;;   recursive procedure for kill thread. Thread find by name.
;; Params:
;;   name     [string]  name of thread to kill
;;   threads  [list]    list of threads
;; Return:
;;   nil or true
(defun kill-thread->iteration (name threads)
  (let
      ((first-thread (first threads))
       (rest-threads (rest threads)))
    (if (equal (bt:thread-name first-thread) name)
        (progn
          (format t "Success: thread killed~%")
          (bt:destroy-thread first-thread)
          t)
        (if (not rest-threads)
            (progn

              (format t "Error: can not found thread. ~%")
              nil)
            (kill-thread->iteration name rest-threads)))))

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
  (let
      ((threads (bt:all-threads)))
    (kill-thread->iteration name threads)))

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
      (format t "Current thread: ~a~%~%" current-thread)
      (format t "Current thread name: ~a~%~%" current-thread-name)
      (format t "All thread: ~% ~{~a~%~}~%" all-threads)
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
  (bt:all-threads))


;; thread-by-name->iteration
;;
;;
;; Description:
;;   recursive procedure for get thread by name
;; Params:
;;   name      [string]    name of thread
;;   threads   [list]      list of threads
;; Return:
;;   thread or nil
(defun thread-by-name->iteration (name threads)
  (let
      ((first-thread (first threads))
       (rest-threads (rest threads)))
    (if (equal name (bt:thread-name first-thread))
        first-thread
        (if (not rest-threads)
            (progn
              (format "Error: can not find thread by name~%")
              nil)
            (thread-by-name->iteration name rest-threads)))))

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
  (let ((threads all-threads))
    (thread-by-name->iteration name threads)))

