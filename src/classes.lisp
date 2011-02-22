;;;; classes.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Classes

;; N-NODES can be deduced but I use it in PRINT-OBJECT and don't want to
;; recalculate every time I print objects to the REPL.
(defclass subtree ()
  ((n-nodes :accessor n-nodes :initarg :n-nodes)
   (tree :accessor tree :initarg :tree)))


;; FITNESS can be deduced but I use it in PRINT-OBJECT and don't want to
;; recalculate every time I print objects to the REPL.
(defclass mote (subtree)
  ((fitness :accessor fitness :initarg :fitness)))


(defclass population ()
  ((best-motes :accessor best-motes :initform (make-array 0 :fill-pointer 0))
   (best-size :accessor best-size :initarg :best-size :initform 10)
   (fitness-function :accessor fitness-function :initarg :fitness-function)
   (motes :accessor motes :initarg :motes)
   (operators :accessor operators :initarg :operators)
   (size :accessor size :initarg :size :initform 100)
   (test-input :accessor test-input :initarg :test-input :initform '(-1 0 1))))


;;; PRINT-OBJECT Methods

(defmethod print-object ((obj mote) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness=~A n-nodes=~A" (fitness obj) (n-nodes obj))))


(defmethod print-object ((obj population) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size=~A" (size obj))))
