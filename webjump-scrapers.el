;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'line-scraper)
(require 'webjump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; builtin scrapes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst wjs/id-attribute "^.*id=\"\\([^\"]+\\)\".*$")

(defconst wjs/builtins

  '(("underscore" .
     (wjs/query!! "underscorejs.org/#"
                  :capture wjs/id-attribute
                  :drop ls/version-number))
    ("backbone" .
     (wjs/query!! "backbonejs.org/#"
                  :capture wjs/id-attribute
                  :drop ls/version-number))

    ("jquery" .
     (wjs/query!! "api.jquery.com/"
                  :capture "^.*href=\"http://api.jquery.com/\\([^/]+\\)/\""))

    ("baconjs" .
     (wjs/query!! "https://github.com/baconjs/bacon.js/tree/master/#"
                  :capture "^.*href=\"#\\([^\"]+\\)\".*$"))))

(defun wjs/query! (base-url &rest transformers)
  "take a url and transformers, query the user for which transformed
dom line to complete on, and return a url that will jump to that
entity.

if base-url is a function, it will be called with no args to produce
the scrape url, and then again with the completion arg to produce the
jump-url.

if base-url is a string, it will be used to scraped and then the
completion value will be appended to the end to produce the jump url.
"
  (let* ((url (if (functionp base-url) (funcall base-url) base-url))
         ;; make the url builder function if base-url is not a
         ;; function.
         (url-builder
          (if (functionp base-url)
              base-url
            (lambda (item) (concat base-url item))))
         ;; get the page as a string
         (page (ls/read-url! url))
         ;; get the transformed dom lines to use for completion
         (completions (apply 'ls/scrape-body page transformers))
         (completion (completing-read "id: " completions)))
    (funcall url-builder completion)))

(defun wjs/query!! (base-url &rest actions)
  "Just like query!, but allows the shorthand dsl (:keep, :drop) that
builds transformers instead of an explicit list of transformers."
  (let ((transformers (apply 'ls/parse-actions actions)))
    (apply 'wjs/query! base-url transformers)))

(provide 'webjump-scrapers)
