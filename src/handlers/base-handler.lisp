(in-package :cse)

(defun base-handler (request response errors)
  (list
   (cons "request" request)
   (cons "response" response)
   (cons "errors" errors)))
