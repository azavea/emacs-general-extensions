;;; line-scraper  -*- lexical-binding: t -*-

;; provide tools for taking a body of text and transforming lines
;; to produce a structured list of output strings, typically for
;; completion libraries.

;; provides a bunch of utilities for promoting regular expressions
;; to transformer functions, and a DSL for expressing transformations
;; succinctly using keywords.

(require 'dash)
(require 'f)
(require 's)

(defconst ls/version-number "\\([0-9]+\.\\)+[0-9]+")

(defconst ls/action-aliases
  '(:keep ls/regexp-keep
    :drop ls/regexp-drop
    :capture ls/regexp-capture))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; regexp utils
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls/regexp-drop (pattern)
  (lambda (value)
    (if (null value) nil
      (if (nth 0 (s-match pattern value)) nil value))))

(defun ls/regexp-keep (pattern)
  (lambda (value)
    (if (null value) nil
    (nth 0 (s-match pattern value)))))

(defun ls/regexp-capture (pattern)
  (lambda (value)
    (if (null value) nil
      (nth 1 (s-match pattern value)))))

;; unused, but maybe useful
(defun ls/capturify-regexp (pattern)
  "Take a match regexp and make it a capture regexp"
  (concat "\\(" pattern "\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; general utils
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls/parse-actions (&rest actions)
  (let ((pairs (-partition 2 actions))
        (fn (lambda (pair)
              (funcall (plist-get ls/action-aliases (car pair)) (nth 1 pair)))))
    (-map fn pairs)))

(defun ls/scrape-body (page &rest transformers)
  "take a body of text as a string, apply each transformer in
succession to each line.  Filter out lines that transformed to nil,
and return a list of the results."
  (let ((tokens! (s-lines page)))
    (dolist (transformer transformers)
      (setq tokens! (-map transformer tokens!)))
    tokens!))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls/read-url! (url)
  "a thin wrapper around curl"
  (let ((curl-command (s-format "curl $0 2>/dev/null" 'elt (list url))))
  (shell-command-to-string curl-command)))

(defalias 'ls/read-file! 'f-read-text)

(defalias 'ls/read-command! 'shell-command-to-string)

(provide 'line-scraper)

