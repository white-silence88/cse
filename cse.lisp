;; cse.lisp
(in-package :cse)
;; run/application
;;
;;
;; Description:
;;   procedure for run application
;; Params:
;;   application-pair   [Pair]   config pair
;;   routes-config      [List]   routes config list
;;   answers-config     [List]   answers config list
;; Returns:
;;    nil
(defun run/application (application-pair routes-config answers-config)
  (let
      ((application-name (car application-pair))
       (application-config (cdr application-pair)))
    (application->start application-name routes-config answers-config)))

;; run/from-folder
;;
;;
;; Description:
;;   public procedure for run prlatform with settings from folder
;; Params:
;;   folder   [String]   fullpath to project folder
;; Returns:
;;   nil
(defun run/from-folder (folder)
  (let
      ((server-config (get/server-config folder))
       (routes-config (get/routes-config folder))
       (answers-config (get/answers-config folder)))
    (loop for application-pair in server-config
          do (run/application application-pair routes-config answers-config))))
