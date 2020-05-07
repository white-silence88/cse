;;;; cse.lisp
;;;; Author:
;;;;   Dmitrii Shevelev <igrave1988@gmail.com>
;;;; Description:
;;;;
(in-package #:cse)



;; woo->run
;;
;; Description:
;;   procedure for run Woo server
;; Params:
;;   nil
;; Return:
;;   nil
(defun woo->run ()
  (woo:run
   (lambda (env) (declare (ignore env))
     '(200 (:content-type "text/plain") ("Hello, world")))))

;; application->start
;;
;; Description:
;;   procedure for run HTTP server application
;; Params:
;;   name [string] name of service
;; Return:
;;   nil
(defun application->start(name)
  (cond ((string-equal name "http")
         (bt:make-thread #'woo->run :name name))
        (t (format t "Not correct name.~%"))))

;; kill-thread->iteration
;;
;;
;; Description:
;; Params:
;; Return:
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
            (format t "Error: can not found thread. ~%")
            (kill-thread->iteration name rest-threads)))))

;; application->kill-thread
;;
;;
;; Description:
;; Params:
;; Return:
(defun application->kill (name)
  (let
      ((threads (bt:all-threads)))
    (kill-thread->iteration name threads)))

;; application->info
;;
;;
;; Description:
;;   procedure for get all threads for application server
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
;; Params:
;; Return:
(defun application->get/threads ()
  (bt:all-threads))

;; thread-by-name->iteration
;;
;;
;; Description:
;; Params:
;; Return:
(defun thread-by-name->iteration (name threads)
  (let
      ((first-thread (first threads))
       (rest-threads (rest threads)))
    (if (equal name (bt:thread-name first-thread))
        first-thread
        (if (not rest-threads)
            (progn
              (format "Error: can not find thread by name")
              nil)
            (thread-by-name->iteration name rest-threads)))))

;; application->get/thread-by-name
;;
;;
;; Description:
;; Params:
;; Return:
(defun application->get/thread-by-name (name)
  (let ((threads all-threads))
    (thread-by-name->iteration name threads)))
