;;; history  -*- lexical-binding: t -*-

;; a data abstraction for a value that has access to its history.
(require 'dash)
(require 'dash-functional)

(defun hist/init (&optional val)
  "Create a history object, optionally with a seed value"
  (if val (list val) nil))

(defun hist/set (hist-obj value)
  "Return a history object with it's latest value set to VALUE"
  (cons value hist-obj))

(defun hist/get (hist-obj &optional index)
  "Get the value of a history object. If index is non-nil, look backward by index."
  (nth (or index 0) hist-obj))

(defun hist/get-when (hist-obj &optional index)
  "Get the value of a hist-obj at a specific point, where 0 is
inception. If INDEX is greater than the history of the object, return
the latest (current) value."
  (car (last hist-obj (+ (or index 0) 1))))

(defun hist/was-at? (hist-obj val &optional index)
  (equal val (hist/get-when hist-obj index)))

(defun hist/fn (fn)
  "take a function whose first arg is a value and modify it to
accept/return history objects"
  (lambda (hist-obj &rest args)
    (hist/set hist-obj (apply fn (hist/get hist-obj) args))))

(defun hist/compose (&rest fns) (apply '-compose (-map 'hist/fn fns)))

(defun hist/filter (pred hist-list)
  "Filter a list of history objects with a predicate function, return a list of history objects."
  (let ((results nil)) 
    (-each hist-list
      (lambda (hist-obj)
        (when (funcall pred (hist/get hist-obj))
          (setq results (cons hist-obj results)))))
    (nreverse results)))

(provide 'history)
