;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'webjump)
(require 'dash)
(require 's)

(let* ((pluck-quoted "\\([^\"]+\\)"))
  (setq wjs/id-capture (concat "^.*id=\"" pluck-quoted "\".*$"))
  (setq wjs/github-href-capture (concat "^.*href=#\"" pluck-quoted "\".*$"))
  (setq wjs/drop-version-numbers "\\([^0-9].*\\)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pure transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wjs/page-to-completions (page &rest patterns)
  "take a webpage as a big string, iterate over each line,
transforming by the capture in the pattern. Return a list of completions."
  (let ((tokens! (s-lines page)))
    (dolist (pattern patterns)
      (setq tokens!
            (-map (-partial 'wjs/regexp-transform pattern) tokens!)))
    tokens!))

(defun wjs/regexp-transform (pattern value)
  "if you provide a pattern, transform a line by the pattern. Otherwise, just return identity."
  (if pattern
      (nth 1 (s-match pattern value))
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wjs/curl! (url)
  (let ((curl-command (s-format "curl $0 2>/dev/null" 'elt (list url))))
  (shell-command-to-string curl-command)))

(defun wjs/query! (base-url &rest patterns)
  (let* ((page (wjs/curl! base-url))
         (args (append (list page) patterns))
         (completions (apply 'wjs/page-to-completions args)))
    (concat base-url "/#" (completing-read "id: " completions))))

(defun wjs/dom-id-query! (base-url) (wjs/query! base-url wjs/id-capture))
(defun wjs/github-doc-query! (base-url) (wjs/query! base-url wjs/github-href-capture))

(provide 'webjump-scrapers)
