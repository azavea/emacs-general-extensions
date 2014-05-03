;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'line-scraper)
(require 'webjump)

(defconst wjs/builtins
  '(("underscore" .
     (wjs/query!! "underscorejs.org" :capture wjs/id-capture :drop ls/version-number))
    ("baconjs" .
     (wjs/query!! "https://github.com/baconjs/bacon.js/tree/master"
                  :capture wjs/github-href-capture))))

(defconst wjs/id-capture "^.*id=\"\\([^\"]+\\)\".*$")
(defconst  wjs/github-href-capture "^.*href=\"#\\([^\"]+\\)\".*$")

(defun wjs/query! (base-url &rest transformers)
  (let* ((page (ls/read-url! base-url))
         (completions (apply 'ls/scrape-body page transformers))
        (id (completing-read "id: " completions)))
    (concat base-url "/#" id)))

(defun wjs/query!! (base-url &rest actions)
  (apply 'wjs/query! base-url (apply 'ls/parse-actions actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin scrapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wjs/builtins (name) (assoc name wjs/builtins))

(provide 'webjump-scrapers)
