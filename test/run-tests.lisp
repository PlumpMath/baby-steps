;;;; run-tests.lisp

(setf *muffled-warnings* 'style-warning)


;;; Packages

(format t "~&Loading lisp-unit...~%")
(load "test/lisp-unit")
(format t "Loading baby-steps.lisp...~%")
(load "src/baby-steps.lisp")

(in-package :baby-steps)
(use-package :lisp-unit)


;;; Parameters

(defparameter ffn1 (lambda (x) (+ x 1)))
(defparameter ffn1-input '(-1 0 2))


;;; Tests

(define-test append1 ()
  (assert-equal '(1 2 3) (append1 '(1 2) 3))
  (assert-equal '(1 2 (3 4)) (append1 '(1 2) '(3 4))))

(define-test calculate-fitness ()
  (assert-equal 0.001 (calculate-fitness '(- =input=  8) ffn1 ffn1-input))
  (assert-equal 1.0   (calculate-fitness '(- =input= -1) ffn1 ffn1-input))
  (assert-equal 1.0   (calculate-fitness '(+ =input=  1) ffn1 ffn1-input))
  (assert-equal 0.125 (calculate-fitness '(+ =input=  2) ffn1 ffn1-input)))

(define-test head ()
  (assert-equal nil (head '()))
  (assert-equal nil (head '(a b c) -1))
  (assert-equal nil (head '(a b c) 0))
  (assert-equal '(a) (head '(a b c)))
  (assert-equal '(a) (head '(a b c) 1))
  (assert-equal '(a b) (head '(a b c) 2))
  (assert-equal '(a b c) (head '(a b c) 3))
  (assert-equal '(a b c) (head '(a b c) 4)))

;(define-test n-nodes ()
;  (assert-equal 1 (n-nodes '()))
;  (assert-equal 2 (n-nodes '(a)))
;  (assert-equal 4 (n-nodes '((a b))))
;  (assert-equal 14  (n-nodes '(1 (2 3) (4 5 6) (7 (8 9))))))

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
