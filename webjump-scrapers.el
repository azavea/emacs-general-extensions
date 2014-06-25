;;; webjump-scrapers  -*- lexical-binding: t -*-

;; a Library for scraping doms for better webjump completion

(require 'line-scraper)
(require 'webjump)

(defconst wjs/builtins
  '(("underscore" .
     (wjs/query! '(:url-builder "underscorejs.org/#"
                   :recipe (:capture "^.*id=\"\\([^\"]+\\)\".*$"
                            :drop "\\([0-9]+\.\\)+[0-9]+"))))
    ("backbone" .
     (wjs/query! '(:url-builder "backbonejs.org/#"
                   :recipe (:capture "^.*id=\"\\([^\"]+\\)\".*$"
                            :drop "\\([0-9]+\.\\)+[0-9]+"))))
    ("baconjs" .
     (wjs/query! '(:url-builder
                   "https://github.com/baconjs/bacon.js/tree/master/#"
                   :recipe
                   (:capture:build-url-on
                    "^.*href=\"#\\([^\"]+\\)\".*$"
                    :fn:complete-on
                    (lambda (arg) (when arg (s-replace "bacon-" "" arg)))))))
    ("jquery" .
     (wjs/query! '(:url-builder "api.jquery.com/"
                   :recipe
                   (:capture
                    "^.*href=\"http://api.jquery.com/\\([^/]+\\)/\""))))))

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

(unless (boundp 'wjs/previous-webjump-sites)
  (setq wjs/previous-webjump-sites (copy-sequence webjump-sites)))
(setq webjump-sites (append wjs/previous-webjump-sites wjs/builtins))

(provide 'webjump-scrapers)
