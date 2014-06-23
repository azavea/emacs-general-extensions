;;; line-scraper -*- lexical-binding: t -*-

(require 'line-scraper)
(require 'test-utils)
(require 'ert)

(defun ls/test/helpers/drop (pattern value)
  (funcall (funcall (plist-get ls/recipe-aliases :drop) pattern) value))

(ert-deftest ls/test/drop ()
  (should (equal (ls/test/helpers/drop "^foo" nil) nil))
  (should (equal (ls/test/helpers/drop "^foo" "foo") nil))
  (should (equal (ls/test/helpers/drop "^foo" "foto") "foto")))

(ert-deftest ls/test/parse-compound-keyword ()
  (should (equal (ls/parse-compound-keyword :foo) '(:foo)))
  (should (equal (ls/parse-compound-keyword :foo:bar) '(:foo :bar))))

(ert-deftest ls/test/parse-action-pair ()
  (let* ((results (ls/parse-action-pair '(:drop "^foo")))
         (transformer (plist-get results :transformer))
         (intermediate-value-keys (plist-get results :intermediate-value-keys)))
    (should (equal nil intermediate-value-keys))
    (should (equal "foto" (funcall transformer "foto"))))
  (let* ((results (ls/parse-action-pair '(:drop:capture-on "^foo")))
         (transformer (plist-get results :transformer))
         (intermediate-value-keys (plist-get results :intermediate-value-keys)))
    (should (equal '(:capture-on) intermediate-value-keys))
    (should (equal "foto" (funcall transformer "foto")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ls/parse-recipe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest ls/test/parse-recipe/basic-recipe ()
  (let ((results (ls/parse-recipe '(:drop "^foo"))))
    (should (equal nil
                   (plist-get results :intermediate-value-indices)))
    (should (equal "foto"
                   (funcall (car (plist-get results :transformers)) "foto"))))
)

(ert-deftest ls/test/parse-recipe/single-intermediate-value ()
  (let ((results (ls/parse-recipe '(:drop "^foo" :keep:capture-on "bar$"))))
    (should (equal '(:capture-on 1)
                   (plist-get results :intermediate-value-indices))))
)
(ert-deftest ls/test/parse-recipe/multiple-intermediate-value-on-transformer ()
  (let ((results (ls/parse-recipe '(:drop "^foo" :keep:complete-on:build-url-on "bar$"))))
    (should (equal '(:complete-on 1 :build-url-on 1)
                   (plist-get results :intermediate-value-indices))))
)

(ert-deftest ls/test/parse-recipe/multiple-intermediate-value-on-transformer ()
  (let ((results (ls/parse-recipe '(:drop "^foo" :keep:complete-on:build-url-on "bar$"))))
    (should (equal '(:complete-on 1 :build-url-on 1)
                   (plist-get results :intermediate-value-indices))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ls/scrape
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest ls/test/scrape ()
  (should (equal '(:final-values (nil "bar" "baz") :intermediate-values nil)
                 (ls/scrape "foo\nbar\nbaz\n" '(:drop "foo"))))
  (should (equal '(:final-values (nil "bar" nil) :intermediate-values (:capture-on (nil "bar" "baz")))
                 (ls/scrape "foo\nbar\nbaz\n" '(:drop:capture-on "foo" :keep "bar"))))
  (should (equal '(:final-values (nil "bar" nil) :intermediate-values (:capture-on (nil "bar" "baz")))
                 (ls/scrape "foo\nbar\nbaz\n" '(:fn (lambda (arg) arg) :drop:capture-on "foo" :keep "bar")))))
