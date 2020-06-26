;;;; cse.asd

(asdf:defsystem #:cse
  :description "Chainsaw Services Engine (CSE) is engine for builds services"
  :author "Dmitrii Shevelev <igrave1988@gmail.com>"
  :license  "GNUv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:woo
               #:cl-async
               #:alexandria
               #:bt-semaphore
               #:jonathan
               #:log4cl
               #:cl-ppcre)
  :components ((:file "package")
               (:file "cse")
               (:file "./src/application")
               (:file "./src/json-answer")
               (:file "./src/json")
               (:file "./src/utils")
               (:file "./src/woo-server")
               (:file "./src/routes")))
