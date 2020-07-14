;; checks.lisp
(in-package :cse)

;; checks/compare-methods-names
;;
;;
;; Descriprion:
;;   procedure for compare method name (string) and some name (from simple array)
;; Params:
;;   method-name       [String]                   name for compare with some name
;;   name-to-compare   [SimpleArray<Charaset>]    name for compare with method name
;; Returns:
;;   result compare two string.
;;   Return True (T) if strings is equals.
;;   Return False (nil) if strings not equals
(defun checks/compare-methods-names (method-name name-to-compare)
  (string= method-name (string name-to-compare)))
