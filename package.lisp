;;;; package.lisp
(defpackage #:cse
  (:use #:cl)
  (:export
   #:application->get/thread-by-name
   #:application->get/threads
   #:application->info/threads
   #:application->start
   #:application->kill))
