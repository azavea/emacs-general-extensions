;;; test-utils  -*- lexical-binding: t -*-

(require 'dash)
(require 'ert)
(require 's)

(defun equal* (&rest vals)
  (-all? 'identity (-map
                    (lambda (val)
                      (equal (car vals) val)) (cdr vals))))

(ert-deftest test/equal* ()
    (should (not (equal* 1 2 3)))
    (should (equal* 1 1 1)))

(defun eval-equal (vals fn1 fn2)
  (-all? 'identity (--map (equal (funcall fn1 it) (funcall fn2 it)) vals)))

(ert-deftest test/eval-equal ()
  "Test that the eval equal helper works"
  (should (eval-equal '("foo" "foso")
                      (-partial 's-replace "-" "")
                      (-partial 's-replace "-" "!!")))
  (should (not (eval-equal '("foo-bar" "foso-bar")
                           (-partial 's-replace "-" "")
                           (-partial 's-replace "-" "!!")))))

(provide 'test-utils)
