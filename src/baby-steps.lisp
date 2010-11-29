;;;; baby-steps.lisp
;;;;
;;;; Usage:
;;;; 1> (defparameter population (create-initial-population 1))
;;;; 2> (evaluate-population population *fitness-function* *input*)

;;; Specials

(defparameter *operators* '(+ - * /))

(defparameter *fitness-function* (lambda (r) (* pi (* r r))))

(defparameter *input* '(1 23 456))


;;; Function

(defun append1 (list object)
  (append list (list object)))


(defun create-initial-population (&optional (size 10))
  (loop repeat size
        collect (random-lambda *operators*)))


(defun evaluate-population (population fitness-function input)
  (loop for i from 0 below (length population)
        for lambda = (elt population i)
        do (format t "=== ~S ===~%" i)
           (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
           (format t "---------|--------------------------~
                               |--------------------------~%")
           (loop for j from 0 below (length input)
                 for in = (elt input j)
                 for out = (run-lambda lambda in)
                 for target = (funcall fitness-function in)
                 do (format t "~8@S | ~24@S | ~24@S~%" in out target))))


(defun random-elt (sequence)
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun random-form (operators)
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (= 0 (random 2))
                            (random-form operators)
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))


(defun random-lambda (operators)
  (append1 '(lambda (=input=))
           (random-form operators)))


(defun run-lambda (lambda input)
  (funcall (eval lambda) input))
