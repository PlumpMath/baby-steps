;;;; pirr.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

;;; Packages

(asdf:oos 'asdf:load-op :baby-steps)

(in-package :baby-steps)


;;; Globals

(defparameter *fitness-fn* (lambda (r) (* pi (* r r))))

(defparameter *test-input* '(-2 0 1 23 456))

(defparameter *operators* '(+ - *))


;;; Functions & Methods

;(defun print-results (tree fitness-function input)
;  (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
;  (format t "---------|--------------------------~
;                      |--------------------------~%")
;  (loop for i from 0 below (length input)
;        for in = (elt input i)
;        for out = (run-tree tree in)
;        for target = (funcall fitness-function in)
;        do (format t "~8@S | ~24@S | ~24@S~%" in out target)))


(defun run-generations (population &optional (generations 10))
  (format t "    best            10%             50%             worst~%")
  (loop repeat generations
        for i from 0
        for lm = (length (motes population))
        do (advance-generation population :method :tournament)
           (format t "[~S] ~15,10E ~15,10E ~15,10E ~15,10E~%" i
                   (fitness (elt (motes population) 0))
                   (fitness (elt (motes population) (floor (* 0.1 lm))))
                   (fitness (elt (motes population) (floor (* 0.5 lm))))
                   (fitness (elt (motes population) (- lm 1))))
           (force-output))
  population)


;;; Main

(defun example-run (&optional population)
  (let ((p (if population
               population
               (create-initial-population *operators* *fitness-fn*
                                          *test-input*))))
    (format t "Running 50 generations...~%")
    (format t "diversity: ~S~%total nodes: ~S~%---~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    (setf p (run-generations p 50))
    (format t "--- best mote ---~%~S~%---~%" (tree (nth-mote p 0)))
    (calculate-fitness (tree (nth-mote p 0)) *fitness-fn* *test-input*
                       :debug t)
    (format t "---~%diversity: ~S~%total nodes: ~S~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    (format t (mkstr "---~%Done.  Call \"(example-run *)\" to continue with "
                     "the same population.~%"))
    p))
