;;;; pirr.lisp
;;;;
;;;; Usage:
;;;; 1> (defparameter popl (create-initial-population *operators*
;;;;                         *fitness-function* *input*))
;;;; 2> (setf popl (run-generations population))

;;; Packages

(asdf:oos 'asdf:load-op :baby-steps)

(in-package :baby-steps)


;;; Globals

(defparameter *fitness-function* (lambda (r) (* pi (* r r))))

(defparameter *input* '(-2 0 1 23 456))

(defparameter *operators* '(+ - * /))


;;; Functions & Methods

(defun run-generations (population &optional (generations 10))
  (loop repeat generations
        for i from 0
        for lm = (length (motes population))
        do (revitalize-population population)
           ;(when (<= (fitness (elt ep 0)) 0)
           ;  (format t "!!! Solution Found !!!~%")
           ;  (return-from run-generations ep))
           (format t "[~S] ~15,10E ~15,10E ~15,10E ~15,10E~%" i
                   (fitness (elt (motes population) 0))
                   (fitness (elt (motes population) (floor (* 0.1 lm))))
                   (fitness (elt (motes population) (floor (* 0.5 lm))))
                   (fitness (elt (motes population) (- lm 1)))))
  population)


;;; Main

(defun example-run ()
  (let ((population (create-initial-population *operators* *fitness-function*
                                               *input*)))
    (format t "Running 50 generations...~%")
    (setf population (run-generations population 50))
    (format t "--- best mote ---~%~S~%---~%"
            (tree (elt (best-motes population) 0)))
    (calculate-fitness (tree (elt (best-motes population) 0))
                       *fitness-function* *input* :debug t)))
