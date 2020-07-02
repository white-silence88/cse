;; json-api-seon.lisp
(in-package :cse)

(defun body-list/success (config)
  (let
      ((message (config/get *message-field* config)))
    (list (cons "msg" message))))

(defun body-list/errors (config)
  (let
      ((message (config/get *message-field* config)))
    (list (cons "msg" message))))

(defun seon-answers->answer (body-list-type answers-config content-type key)
  (let*
      ((property-config (config/get key answers-config))
       (code (config/get *code-field* property-config))
       (body-list (cond
                   ((string= "error" body-list-type)
                    (body-list/errors property-config))
                   (t
                    (body-list/success property-config)))))
    (get-answer code content-type body-list)))

(defun seon-answers/errors (client-errors-config content-type key)
  (seon-answers->answer "error" client-errors-config content-type key))

(defun seon-answers/errors->not-found (client-errors-config content-type)
  (seon-answers/errors client-errors-config content-type *not-found-config-key*))

(defun seon-answers/success (success-config content-type key)
  (seon-answers->answer "success" success-config content-type key))
