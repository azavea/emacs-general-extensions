;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'line-scraper)
(require 'webjump)

(defconst wjs/id-attribute "^.*id=\"\\([^\"]+\\)\".*$")
(defconst wjs/version-number "\\([0-9]+\.\\)+[0-9]+")
(defconst wjs/jashkenas-web-recipe
  (list :capture wjs/id-attribute
        :drop wjs/version-number))

(defconst wjs/underscore-web-recipe
  (list :url-builder "underscorejs.org/#"
        :recipe wjs/jashkenas-web-recipe))

(defconst wjs/backbone-web-recipe
  (list :url-builder "backbonejs.org/#"
        :recipe wjs/jashkenas-web-recipe))

(defconst wjs/baconjs-web-recipe
  '(:url-builder
    "https://github.com/baconjs/bacon.js/tree/master/#"
    :recipe
    (:capture:build-url-on
     "^.*href=\"#\\([^\"]+\\)\".*$"
     :fn:complete-on
     (lambda (arg) (when arg (s-replace "bacon-" "" arg))))))

(defconst wjs/jquery-web-recipe
  '(:url-builder "api.jquery.com/"
    :recipe
    (:capture
     "^.*href=\"http://api.jquery.com/\\([^/]+\\)/\"")))

(add-to-list 'webjump-sites '("underscore" . (wjs/query! wjs/underscore-web-recipe)))
(add-to-list 'webjump-sites '("backbone" . (wjs/query! wjs/backbone-web-recipe)))
(add-to-list 'webjump-sites '("baconjs" . (wjs/query! wjs/baconjs-web-recipe)))
(add-to-list 'webjump-sites '("jquery" . (wjs/query! wjs/jquery-web-recipe)))

(defun wjs/functify-url-builder (url-builder)
"a URL-BUILDER can be a string, or a function.

If URL-BUILDER is a function, it is called with no args to produce a
 base url, and with one arg to produce a complete url."
(if (functionp url-builder)
    url-builder
  (lambda (&optional arg) (concat url-builder arg))))

(defun wjs/query! (query)
" Takes a plist arglist, QUERY.

QUERY can contain the following keys:
- :url-builder see `wjs/functify-url-builder' for details on url-builders.
- :prompt      what to prompt for with completions, defaults to \"id: \"
- :recipe      see `ls/scrape' for details on recipes."
(let* ((url-builder (wjs/functify-url-builder (plist-get query :url-builder)))
       (url (funcall url-builder))
       (page (ls/read-url! url))
       (scrape-data (ls/scrape page (plist-get query :recipe)))
       (final-values (plist-get scrape-data :final-values))
       (intermediate-values (plist-get scrape-data :intermediate-values))
       (completion-list (or (plist-get intermediate-values :complete-on) final-values))
       (url-arg-list (or (plist-get intermediate-values :build-url-on) final-values))
       (prompt (or (plist-get query :prompt) "id: "))
       (completion (ido-completing-read prompt (-filter 'identity completion-list)))
       (index (-elem-index completion completion-list))
       (url-arg (nth index url-arg-list)))
  (funcall url-builder url-arg)))

(provide 'webjump-scrapers)
