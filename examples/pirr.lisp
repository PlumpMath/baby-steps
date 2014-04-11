;;;; pirr.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.
;;;;
;;;; Description: find the program that best calculates the area of a circle.

;;; Packages

(ql:quickload :baby-steps)
(in-package :baby-steps)


;;; Globals

(defparameter *fitness-fn* (lambda (r) (* pi (* r r))))

(defparameter *test-input* '(-2 0 1 23 456))

(defparameter *operators*
  '((:operator +   :type number :arity :max)
    (:operator -   :type number :arity :max)
    (:operator *   :type number :arity :max)
    ;(:operator /   :type number :arity :max)  ; divide-by-zero
    (:operator cos :type number :arity 1)
    (:operator sin :type number :arity 1)))


;; Terminals

(defun random-double-float-0-10 ()
  (random 10.0d0))


(defun random-float-0-10 ()
  (random 10.0))


(defun random-int-0-10 ()
  (random 11))


(defparameter *terminals*
  `((:terminal =input=                     :type number :input t)
    (:terminal ,#'random-double-float-0-10 :type double-float)
    (:terminal ,#'random-float-0-10        :type float)
    (:terminal ,#'random-int-0-10          :type integer)))


;;; Functions

(defun calculate-fitness (mote)
  (unless (fn mote)
    (setf (fn mote) (make-function (tree mote) *terminals*)))
  (loop for ti in *test-input*
        for ref = (funcall *fitness-fn* ti)
        for res = (handler-case (funcall (fn mote) ti)
                    (error () (return-from calculate-fitness nil)))
        sum (abs (- ref res)) into aggregate-diff
        ;; basic accumulation of diffs
        ;finally (return (/ 1 aggregate-diff))))
        ;; bias towards fewer nodes
        finally (return (/ 1 (sqrt aggregate-diff) (sqrt (n-nodes mote))))))


(defun make-valid-mote ()
  (loop with fitness = nil
        until fitness
        for new-mote = (create-mote *operators* *terminals*)
        do (setf fitness (calculate-fitness new-mote))
           (when fitness
             (setf (fitness new-mote) fitness)
             (return-from make-valid-mote new-mote))))


(defun replace-invalid-motes (population)
  (loop for m across (motes population)
        for i from 0
        when (null (fitness m))
          do (setf (aref (motes population) i) (make-valid-mote))))


(defun set-fitness-for-whole-population (population)
  (loop for m across (motes population)
        do (setf (fitness m) (calculate-fitness m))))


(defun update-population (population)
  "Calculates the fitness for the whole population, replaces any invalid
  motes with valid ones and sorts the motes."
  (set-fitness-for-whole-population population)
  (replace-invalid-motes population)
  (sort-motes population))


(defun print-results (mote)
  "Prints results for MOTE compared to *FITNESS-FN*."
  (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
  (format t "---------|--------------------------~
                      |--------------------------~%")
  (loop for in in *test-input*
        for out = (handler-case (funcall (fn mote) in)
                    (error () nil))
        for target = (funcall *fitness-fn* in)
        do (format t "~8@S | ~24@S | ~24@S~%" in out target)))


(defun run-generations (population &key (generations 10) (debug nil))
  (when debug
    (format t " gen | best            | 10%             | 50%             | ~
               worst~%-----+-----------------+-----------------+-----------~
               ------+----------------~%"))
  (loop repeat generations
        for i from 0
        for lm = (length (motes population))
        do (set-fitness-for-whole-population population)
           (advance-generation population :method :tournament)
           (replace-invalid-motes population)
           (sort-motes population)
           (when debug
             (format t "~4D | ~15,10E | ~15,10E | ~15,10E | ~15,10E~%"
                     i
                     (fitness (elt (motes population) 0))
                     (fitness (elt (motes population) (floor (* 0.1 lm))))
                     (fitness (elt (motes population) (floor (* 0.5 lm))))
                     (fitness (elt (motes population) (- lm 1))))
             (force-output)))
  population)


(defun run-generations-stats (population &key (generations 10))
  (loop repeat generations
        for i from 0
        for lm = (length (motes population))
        do (set-fitness-for-whole-population population)
           (advance-generation population :method :tournament)
           (replace-invalid-motes population)
           (sort-motes population)
        collect (list (fitness (nth-mote population 0))
                      (- (fitness (nth-mote population 0))
                         (fitness (nth-mote population
                                            (ceiling (size population) 2)))))))


(defun stats (&optional (samples 10))
  (format t "=== stats with ~D samples ===~%" samples)
  (loop with settings = '((  64 64) (  64 256) (  64 1024)
                          ( 256 64) ( 256 256) ( 256 1024)
                          (1024 64) (1024 256) (1024 1024))
        for setting in settings
        for generations = (first setting)
        for size = (second setting)
        for results = (loop repeat samples
                            for i from 1
                            do (format t "[~D/~D] size=~D generations=~D~%"
                                       i samples size generations)
                            collect (run-generations-stats
                                     (create-population *operators* *terminals*
                                                        :size size)
                                     :generations generations))
        for file = (mkstr "fitness-size-" size "-gens-" generations ".dat")
        do (with-open-file (f file :direction :output :if-exists :supersede)
             (format t "Writing results to ~A...~%" file)
             (loop for i from 0 below generations
                   with sum-fitness = 0
                   with sum-dfitn = 0
                   do (loop for result in results
                            for fitness = (first (elt result i))
                            for dfitn = (second (elt result i))
                            do (incf sum-fitness fitness)
                               (incf sum-dfitn dfitn))
                      (format f "~D ~F ~F~%"
                              i
                              (/ sum-fitness samples)
                              (/ sum-dfitn samples))))))


;;; Main

(defun reload ()
  (load "examples/pirr.lisp"))


(defun main (&key (population nil) (generations 128) (size 128))
  (let ((p (if population
               population
               (create-population *operators* *terminals* :size size))))
    (format t "Running ~D generations...~%" generations)
    (format t "diversity: ~S~%total nodes: ~S~%---~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    (setf p (run-generations p :generations generations :debug t))
    (update-population p)
    (format t "--- best mote (fitness: ~,10E) ---~%~S~%"
            (calculate-fitness (nth-mote p 0))
            (tree (nth-mote p 0)))
    (format t "---~%diversity: ~S~%total nodes: ~S~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    (print-results (nth-mote p 0))
    p))
