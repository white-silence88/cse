;; configs.lisp
(in-package #:cse)
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
   ((string= type *default-redirection-field*)
    *default-redirection-config-name*)
   ((string= type *default-informational-field*)
    *default-informational-config-name*)
   ((string= type *default-server-errors-field*)
    *default-server-erorrs-config-name*)
   ((string= type *default-client-errors-field*)
    *default-client-errors-config-name*)
   ((string= type *default-success-field*)
    *default-success-config-name*)
   ((string= type *default-server-field*)
    *default-server-config-name*)
   ((string= type *default-routes-field*)
    *default-routes-config-name*)
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

(defun get/configs->by-filenames (folder filenames)
  (map 'list
       (lambda (filename)
         (cons filename (from-folder/json->tree folder filename))) filenames))


(defun get/answers-config (folder)
  (let
      ((filenames
        (list
         "success"
         "redirection"
         "informational"
         "client-errors"
         "server-errors")))
    (get/configs->by-filenames folder filenames)))

(defun get/routes-config (folder)
  (from-folder/json->tree folder "routes"))

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

;; request/get<-check
;;
;;
;; Description:
;;   procedure for check name of finded property and property name from pair
;; Params:
;;   property   [String] name of property
;;   item       [Pair]   pair with property name and value
;; Returns:
;;   result check (T or nil)
(defun config/get<-check (property item)
  (string= property (car item)))

;; config/get
;;
;;
;; Description:
;;   procedure for get property from configuration list
;; Params:
;;   property    [String]    name of property in config
;;   config      [List]      list of config properties
;; Returns:
;;   value of config property or nil
(defun config/get (property config)
  (let
      ((pair (find property config :test #'config/get<-check)))
    (cond
     ((not pair) nil)
     (t (cdr pair)))))
