(in-package :cse)

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
