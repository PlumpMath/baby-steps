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
  ((n-nodes :reader n-nodes :initarg :n-nodes)
   (tree :reader tree :initarg :tree)))


;; FITNESS can be deduced but I use it in PRINT-OBJECT and don't want to
;; recalculate every time I print objects to the REPL.
(defclass mote (subtree)
  ((fitness :reader fitness :initarg :fitness)
   ;; only used in ADVANCE-GENERATION-FITNESS-PROPORTIONATE
   (normalised-fitness :accessor normalised-fitness :initform nil)))


(defclass population ()
  ((diversity :accessor diversity :initform 1.0)  ; 100%
   (fitness-fn :accessor fitness-fn :initarg :fitness-fn)
   (motes :accessor motes :initarg :motes)
   (operators :accessor operators :initarg :operators)
   (size :accessor size :initarg :size :initform 100)
   (terminals :accessor terminals :initarg :terminals)
   (test-input :accessor test-input :initarg :test-input :initform '(-2 0 1))))


;;; PRINT-OBJECT Methods

(defmethod print-object ((obj mote) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness=~A n-nodes=~A" (fitness obj) (n-nodes obj))))
    ;(format stream "fitness=~A (~A) n-nodes=~A"
    ;        (fitness obj) (normalised-fitness obj) (n-nodes obj))))


(defmethod print-object ((obj population) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size=~A" (size obj))))
