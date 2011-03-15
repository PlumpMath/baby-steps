;;;; sound-gen.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.
;;;;
;;;; Requires "aplay" to be installed on your system and findable in your path.

;;; Packages

(asdf:oos 'asdf:load-op :baby-steps)

(in-package :baby-steps)


;;; Globals

(defparameter *aplay-proc* (run-program "aplay" nil :input stream :search t
                                        :wait nil))
(defparameter *aplay* (process-input aplay-proc))

;; not applicable since the user decides the fitness
(defparameter *fitness-fn* (lambda (x) (declare (ignore x)) 1.0))

(defparameter *test-input* '(-2 0 1))

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
    (:terminal ,pi                         :type double-float)  ; cheating
    (:terminal ,#'random-double-float-0-10 :type double-float)
    (:terminal ,#'random-float-0-10        :type float)
    (:terminal ,#'random-int-0-10          :type integer)))


;;; Main

(defun example-run (&key (population nil) (runs 3) (size 10))
  (let ((p (if population
               population
               (create-initial-population *operators* *terminals* *fitness-fn*
                                          *test-input* :size size))))
    (format t "Running ~D generations...~%" runs)
    (format t "diversity: ~S~%total nodes: ~S~%---~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    (loop repeat runs
          do (review-motes p)
             (advance-generation p))
    (format t "--- best mote ---~%~S~%" (tree (nth-mote p 0)))
    (format t "---~%diversity: ~S~%total nodes: ~S~%"
            (population-diversity p)
            (loop for mote across (motes p) sum (n-nodes mote)))
    p))


(defun play-mote (mote &key (duration 5.0) (sample-rate 8000))
  (play-tree (tree mote) duration sample-rate))


(defun play-tree (tree &key (duration 5.0) (sample-rate 8000))
  (loop with fn = (make-function tree *terminals*)
        for time from 0.0 to duration by (/ 1.0 sample-rate)
        for value = (let ((val (floor (* 127 (+ (funcall fn time) 1)))))
                      (cond ((< val 0) 0)
                            ((> val 255) 255)
                            (t val)))
        do (write-byte value aplay)
           (finish-output aplay)))


;; This sucks.  I really need to read up on sound buffers: getting their
;; status, nuking them, etc.
(defun review-motes (population)
  (loop for mote across (motes population)
        for i from 0
        do (when (member "play-sound" (sb-thread:list-all-threads)
                         :key #'sb-thread:thread-name :test #'string=)
             (format t "Waiting for last sound to finish...~%")
             (loop while (member "play-sound" (sb-thread:list-all-threads)
                                 :key #'sb-thread:thread-name :test #'string=)
                   do (sleep 0.1)))
           (format t "[~D] ~S~%score?: " i (tree mote))
           (sb-thread:make-thread (lambda () (play-mote mote))
                                  :name "play-sound")
           (let ((score (parse-integer (read-line))))
             (setf (slot-value mote 'fitness) (case score
                                                (1 1.00)
                                                (2 0.75)
                                                (3 0.50)
                                                (4 0.25)
                                                (5 0.00))))))
