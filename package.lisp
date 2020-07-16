;;;; package.lisp
(defpackage #:cse
  (:use #:cl)
  (:export
   ;;; Main API
   #:run/from-folder
   ;;; API for work with JSON files
   #:json-file->>tree
   #:json-file->>routes-tree
   #:jsonify
   ;;; Procedure for work with server answer
   #:get-answer
   ;;; API for work with routes
   #:routes-config->>routes-map
   #:routes-map/find
   ;; API for work with default adapter
   #:env->request
   ;;; API for work with WOO server
   #:woo->>request
   ;;; API for get JSON API answer (simple SEON standard)
   #:seon-answers/success
   #:seon-answers/errors
   #:seon-answers/errors->not-found
   ;;; Procedures from utils
   #:config/get
   ;;; API for get some from configs
   #:get/server-config
   #:get/routes-config
   #:get/answers-config
   ;;; API for work with applications
   #:application->get/thread-by-name
   #:application->get/threads
   #:application->info/threads
   #:application->start
   #:application->kill))

