;;;; run-tests.lisp

(setf *muffled-warnings* 'style-warning)


;;; Packages

(format t "~&Loading lisp-unit...~%")
(load "test/lisp-unit")
(format t "Loading baby-steps.lisp...~%")
(load "src/baby-steps.lisp")

(in-package :baby-steps)
(use-package :lisp-unit)


;;; Tests

(define-test append1 ()
  (assert-equal '(1 2 3) (append1 '(1 2) 3))
  (assert-equal '(1 2 (3 4)) (append1 '(1 2) '(3 4))))

(define-test n-nodes ()
  (assert-equal 1 (n-nodes '()))
  (assert-equal 2 (n-nodes '(a)))
  (assert-equal 4 (n-nodes '((a b))))
  (assert-equal 14  (n-nodes '(1 (2 3) (4 5 6) (7 (8 9))))))

(define-test replace-node ()
  (assert-equal '(4 2 3) (replace-node '(1 2 3) 1 4))
  (assert-equal '(1 (4 5) 3) (replace-node '(1 (2 3) 3) 2 '(4 5)))
  (assert-equal '(1 2 (4 (5 6 (7 8 9))))
                (replace-node '(1 2 (3 (4 5))) 3 '(4 (5 6 (7 8 9))))))


;;; Run the tests.

(format t "Running tests...~%")
(run-tests)
(format t "~&")
(cl-user::quit)
