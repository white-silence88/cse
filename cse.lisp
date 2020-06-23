;;;; cse.lisp
(in-package :cse)

;; Define default name for server config file
(defparameter *default-server-config-name* "config.json")
;; Define default name for routes config file
(defparameter *default-routes-config-name* "routes.json")

;; get/server-config
;;
;;
;; Description:
;;   procedure for get server config
;; Params:
;;   folder   [String]   fullpath to project folder
;; Returns:
;;   config list for server config
(defun get/server-config (folder)
  (let
      ((fullpath (concatenate 'string folder *default-server-config-name*)))
    (json-file->>tree fullpath)))

;;get/routes-config
;;
;;
;; Description:
;;   procedure for get routes config list
;; Params:
;;   folder   [String]   fullpath to project folder
;; Returns:
;;   config list for routes config
(defun get/routes-config (folder)
  (let
      ((fullpath (concatenate 'string folder *default-routes-config-name*)))
    (json-file->>routes-tree fullpath)))

;; run/application
;;
;;
;; Description:
;;   procedure for run application
;; Params:
;;   application-pair   [Pair]   
;; Returns:
;;    nil
(defun run/application (application-pair routes-config)
  (let
      ((application-name (car application-pair))
       (application-config (cdr application-pair)))
    (application->start application-name)))

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
       (routes-config (get/routes-config folder)))
    (loop for application-pair in server-config
          do (run/application application-pair routes-config))))
