;; json-answer.lisp
(in-package :cse)

;; answer->>jsonify
(defun answer->>jsonify (raw-data)
  (jonathan:to-json raw-data :from :alist))
