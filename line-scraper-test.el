(require 'line-scraper)
(require 'test-utils)
(require 'ert)


(ert-deftest ls/regexp-drop ()
  (should (equal (funcall (ls/regexp-drop "^foo") nil) nil))
  (should (equal (funcall (ls/regexp-drop "nil") nil) nil))
  (should (equal (funcall (ls/regexp-drop "^foo") "foo") nil))
  (should (equal (funcall (ls/regexp-drop "^foo") "foso") "foso")))

(ert-deftest ls/parse-actions ()
  (should (eval-equal '("foo" "foso" "1.2.3" nil)
                      (ls/regexp-drop "^foo")
                      (car (ls/parse-actions :drop "^foo")))))


(ert-deftest ls/scrape-body ()
  (let ((f (lambda (arg) (concat arg "BAZ!"))))
  (should (equal (ls/scrape-body "foo\nbar") '("foo" "bar")))
  (should (equal (ls/scrape-body "foo\nbar" f) '("fooBAZ!" "barBAZ!")))
))
