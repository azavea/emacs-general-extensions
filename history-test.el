;;; history-test  -*- lexical-binding: t -*-

(require 'history)
(require 'ert)

(ert-deftest hist/init ()
  (should (equal (hist/init) nil))
  (should (equal (hist/init 1) '(1)))
)

(ert-deftest hist/set ()
  (should (equal (hist/set (hist/init) 1) (hist/init 1)))
  (should (equal (hist/set '(5 4 3 2 1) 6) '(6 5 4 3 2 1)))
  (should (equal (hist/set nil 1) '(1)))
)

(ert-deftest hist/get-when ()
  (should (equal (hist/get-when '(1 2 3)) 3))
  (should (equal (hist/get-when '(1 2 3) 0) 3))
  (should (equal (hist/get-when '(1 2 3) 1) 2))
  (should (equal (hist/get-when '(1 2 3) 2) 1))
  (should (equal (hist/get-when '(1 2 3) 3) 1))
)

(ert-deftest hist/was-at? ()
  (let ((hist-obj '(1 2 3)))
  ;; test with no index
  (should (hist/was-at? hist-obj 3))
  (should-not (hist/was-at? hist-obj 2))

  ;; test with index
  (should (hist/was-at? hist-obj 3 0))
  (should (hist/was-at? hist-obj 2 1))
  (should-not (hist/was-at? hist-obj 2 2))
  ))

(ert-deftest hist/fn ()
    (-each (list 'abs (-partial '+ 5))
      (lambda (fn)
        (-each '(1 2 3)
          (lambda (val)
            (should (equal 
                     (funcall fn val)
                     (hist/get (funcall (hist/fn fn) (hist/init val))))))))))

(ert-deftest hist/compose ()
  (should (equal -9 (hist/get (funcall (hist/compose (-rpartial '- 10) 'abs) (hist/init 1))))))

(ert-deftest hist/filter ()
  (let ((h1 '(9 8 7))
        (h2 '(20 19 18))
        (h3 '(15 100 1000))
        (h4 '(1 23)))
    (should (equal (hist/filter (lambda (n) (> n 10)) (list h1 h2 h3 h4)) (list h2 h3)))
    )
  )
