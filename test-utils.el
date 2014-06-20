;;; test-utils  -*- lexical-binding: t -*-

(require 'dash)
(require 'ert)

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

(provide 'test-utils)
