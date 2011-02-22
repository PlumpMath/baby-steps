;;;; common.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.
;;;;
;;;; This file contains function for which one actually ought to use an
;;;; external utilities package like Alexandria or Arnesi.

(in-package :baby-steps)


;;; Functions

(defun append1 (list object)
  "Shorthand for: (APPEND LIST (LIST OBJECT))."
  (append list (list object)))


(defun head (sequence &optional (amount 1))
  "Returns AMOUNT elements from the start of SEQUENCE.  If SEQUENCE is shorter
  than AMOUNT it will return SEQUENCE."
  (if (<= amount 0)
      nil
      (if (< (length sequence) amount)
          sequence
          (subseq sequence 0 amount))))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))
