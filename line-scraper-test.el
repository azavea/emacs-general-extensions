(require 'line-scraper)
;; todo: resolve dependencies for bash,
;;       probably by using a cask file

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test utils
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equal* (&rest vals)
  (-all? 'identity (-map
                    (lambda (val)
                      (equal (car vals) val)) (cdr vals))))

(defun eval-equal (vals fn1 fn2)
  (--map (equal (funcall fn1 it) (funcall fn2 it)) vals))

(ert-deftest equal* ()
    (should (not (equal* 1 2 3)))
    (should (equal* 1 1 1)))

(ert-deftest eval-equal ()
  "Test that the eval equal helper works"
  (should (eval-equal '("foo" "foso")
                      (ls/regexp-drop "^foo")
                      (ls/regexp-drop "^foo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test utils
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
