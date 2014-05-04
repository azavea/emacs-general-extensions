;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'line-scraper)
(require 'webjump)

(defconst wjs/builtins
  '(("underscore" .
     (wjs/query!! "underscorejs.org" wjs/hash-url-builder :capture wjs/id-capture :drop ls/version-number))

    ("jquery" .
     (wjs/query!! "api.jquery.com"
                  (lambda (base-url item) (concat base-url "/" item))
                  :capture "^.*href=\"http://api.jquery.com/\\([^/]+\\)/\""))

    ("baconjs" .
     (wjs/query!! "https://github.com/baconjs/bacon.js/tree/master"
                  wjs/hash-url-builder
                  :capture wjs/github-href-capture))))

(defconst wjs/id-capture "^.*id=\"\\([^\"]+\\)\".*$")
(defconst  wjs/github-href-capture "^.*href=\"#\\([^\"]+\\)\".*$")
(defconst wjs/hash-url-builder (lambda (base-url dom-id) (concat base-url "/#" dom-id)))

(defun wjs/query! (base-url url-builder &rest transformers)
  (let* ((page (ls/read-url! base-url))
         (completions (apply 'ls/scrape-body page transformers))
         (completion (completing-read "id: " completions)))
    (funcall url-builder base-url completion)))

(defun wjs/query!! (base-url url-builder &rest actions)
  (apply 'wjs/query! base-url url-builder (apply 'ls/parse-actions actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin scrapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wjs/builtins (name) (assoc name wjs/builtins))

(provide 'webjump-scrapers)
