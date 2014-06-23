;;; line-scraper -*- lexical-binding: t -*-

;; provide tools for taking a body of text and transforming lines to
;; produce a structured list of output strings, typically for
;; completion libraries.

;; provides a bunch of utilities for promoting regular expressions to
;; transformer functions, and a DSL for expressing transformations
;; succinctly using keywords.

;; :recipe should be a plist, with keyword args :capture, :keep,
;; :drop, :fn.  if the arg is a :fn, it will be applied. otherwise, it
;; should be a regexp to lift into a fn.  additionally, any of these
;; keywords can be appended like so, to complete-on, or to build
;; arg-on: :capture:complete-on <pattern>

(require 'history)
(require 'dash)
(require 'dash-functional)
(require 'f)
(require 's)

(defconst ls/recipe-aliases
  '(:fn (lambda (fn) fn)
    :keep (lambda (pattern)
            `(closure
              ((pattern . ,pattern) t) (value)
              (if (null value) nil
                (nth 0 (s-match pattern value)))))
    :drop (lambda (pattern)
            `(closure
              ((pattern . ,pattern) t) (value)
              (if (null value) nil
                (if (nth 0 (s-match pattern value)) nil value))))
    :capture (lambda (pattern)
               `(closure
                 ((pattern . ,pattern) t) (value)
                 (if (null value) nil
                   (nth 1 (s-match pattern value)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; general utils
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls/parse-compound-keyword (keyword)
  "turn a keyword into a list of keywords, so that :foo becomes (:foo)
and :foo:bar becomes (:foo :bar)"
  (--map (intern (concat ":" it))
         (s-split ":" (symbol-name keyword) :omit-empty-strings)))

(defun ls/parse-action-pair (pair)
  (let* ((keywords (ls/parse-compound-keyword (car pair)))
         (keyword (car keywords))
         (intermediate-value-keys (cdr keywords))
         (pattern (nth 1 pair))
         (pattern-lifter (plist-get ls/recipe-aliases keyword))
         (transformer (funcall pattern-lifter pattern)))
    (list :transformer transformer
          :intermediate-value-keys intermediate-value-keys)))

(defun ls/parse-recipe (recipe)
  "takes a recipe, returns a plist of transformers, and intermediate value indices.

ex:
\\(ls/parse-recipe '(:drop foo :drop:capture-on bar :fn 'baz)) =>

\\(:transformers (drop-foo-lambda 'drop-bar-lambda 'baz)
 :intermediate-value-indices (:capture-on 1))
"
  ;; split the plist pairs of the recipe
  (let* ((pairs (-partition 2 recipe))
         (pair-results (-map 'ls/parse-action-pair pairs))

         ;; intialize components of return type
         (transformers (list))
         (intermediate-value-indices (list))

         ;; create iteration index
         (pair-index 0))

    ;; iterate over each parsed pair, add the transformers and value indices, if any.
    (dolist (pair-result pair-results)
      (let ((transformer (plist-get pair-result :transformer))
            (intermediate-value-keys (plist-get pair-result :intermediate-value-keys)))
        (setq transformers (cons transformer transformers))
        (when intermediate-value-keys
          (dolist (intermediate-value-key intermediate-value-keys)
            (setq intermediate-value-indices
                  (plist-put intermediate-value-indices intermediate-value-key pair-index)))))
      (setq pair-index (+ 1 pair-index)))

    ;; return transformers and value indices as a plist
    (list :transformers (nreverse transformers)
          :intermediate-value-indices intermediate-value-indices)))

(defun ls/scrape (body recipe)
  "Take a page and a recipe. Return a plist of the finished,
  transformed value according to the recipe, as well as any
  intermediate lists that were specified by compound keyword arguments
  in the recipe."
  (let* ((parsed-recipe (ls/parse-recipe recipe))
         (transformers (plist-get parsed-recipe :transformers))
         (intermediate-value-indices
          (plist-get parsed-recipe :intermediate-value-indices))
         (line-histories (-map 'hist/init (-filter 's-present? (s-lines body))))
         (transformed-histories (-map (apply 'hist/compose (nreverse transformers)) line-histories))
         (final-values (-map 'hist/get transformed-histories))
         (intermediate-values (list)))
    (--each (-partition 2 intermediate-value-indices)
      (let* ((key (car it))
             (value (nth 1 it))
             (intermediate-value-list (list)))
        (dolist (transformed-history transformed-histories)
          (setq intermediate-value-list (cons (hist/get-when transformed-history (1+ value)) intermediate-value-list)))
        (setq intermediate-values (plist-put intermediate-values key (nreverse intermediate-value-list)))
        ))
    (list :final-values final-values :intermediate-values intermediate-values)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IO
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ls/read-url! (url)
  "a thin wrapper around curl"
  (let* ((curl-command (s-format "curl $0 2>/dev/null" 'elt (list url)))
         (body (shell-command-to-string curl-command)))
    (copy-sequence body)))

(defalias 'ls/read-file! 'f-read-text)

(defalias 'ls/read-command! 'shell-command-to-string)

(provide 'line-scraper)
