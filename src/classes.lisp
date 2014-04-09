;;;; classes.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Classes

(defclass subtree ()
  ((n-nodes :accessor n-nodes :initarg :n-nodes)
   (tree :accessor tree :initarg :tree)))


(defclass mote (subtree)
  ((fitness :accessor fitness :initarg :fitness :initform nil)
   (fn :accessor fn :initarg fn :initform nil)))


(defclass population ()
  ((diversity :accessor diversity :initform 1.0)  ; 100%
   (motes :accessor motes :initarg :motes)
   (operators :accessor operators :initarg :operators)
   (size :accessor size :initarg :size :initform 100)
   (terminals :accessor terminals :initarg :terminals)))


;;; PRINT-OBJECT Methods

(defmethod print-object ((obj mote) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness=~A n-nodes=~A" (fitness obj) (n-nodes obj))))


(defmethod print-object ((obj population) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "diversity=~A size=~A" (diversity obj) (size obj))))
