;;;; cse.lisp
(in-package :cse)

;; Define default name for server config file
(defparameter *default-server-config-name* "config.json")
;; Define default name for routes config file
(defparameter *default-routes-config-name* "routes.json")
;; Define default name for success config file
(defparameter *default-success-config-name* "success.json")
;; Define default name for client errors file
(defparameter *default-client-errors-config-name* "client-errors.json")
;; Define default name for server errors file
(defparameter *default-server-erorrs-config-name* "server-errors.json")
;; Define default name for informational answers file
(defparameter *default-informational-config-name* "informational.json")
;; Define default name for redirection answer field
(defparameter *default-redirection-config-name* "redirection.json")

;; by-folder/get-fullpath
;;
;;
;; Description:
;;   procedure for get fullpath to file by folderpath and filename
;; Params:
;;   folder     [String]   fullpath to project folder
;;   filename   [String]   name of files
;; Returns:
;;   fullpath to file
(defun by-folder/get-fullpath (folder filename)
  (cond
    ((not filename) nil)
    (t (concatenate 'string folder filename))))

;; get-filename-by-type
;;
;;
;; Description:
;;   procedure for get default filename
;; Params:
;;   type    [String]   name of type config
;; Returns:
;;   filename or nil
(defun get-filename-by-type (type)
  (cond
    ((string= type "redirection") *default-redirection-config-name*)
    ((string= type "informational") *default-informational-config-name*)
    ((string= type "server-errors") *default-server-erorrs-config-name*)
    ((string= type "client-errors") *default-client-errors-config-name*)
    ((string= type "success") *default-success-config-name*)
    ((string= type "server") *default-server-config-name*)
    ((string= type "routes") *default-routes-config-name*)
    (t nil)))

;; from-folder/json->tree
;;
;;
;; Description:
;;   procedure for get fullpath to file by folder fullpath and type of config
;; Params:
;;   folder    [String]   fullpath to folder
;;   type      [String]   type of config
;; Returns:
;;   fullpath to file or nil
(defun from-folder/json->tree (folder type)
  (let*
      ((fullpath (by-folder/get-fullpath folder (get-filename-by-type type))))
    (cond
      ((not fullpath) nil)
      (t (if (string= type "routes")
             (json-file->>routes-tree fullpath)
             (json-file->>tree fullpath))))))

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
  (from-folder/json->tree folder "server"))

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
  (from-folder/json->tree folder "routes"))

;; get/answers-config
;;
;;
;; Description:
;;   procedure for get answers configs
;; Params:
;;   folder   [String]   fullpath to folder
;; Returns:
;;   list of configurations for answers
(defun get/answers-config (folder)
  (let
      ((filenames (list "success" "redirection" "informational" "client-errors" "server-errors")))
    (map 'list
         (lambda (filename)
           (cons filename (from-folder/json->tree folder filename))) filenames)))

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
