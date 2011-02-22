;;;; creation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions

(defun copy-mote (mote)
  "Returns a new MOTE instance with the same slot values as MOTE."
  (make-instance 'mote :fitness (fitness mote) :n-nodes (n-nodes mote)
                 :tree (tree mote)))  ; XXX: (copy-seq (tree mote))?


(defun create-initial-population (operators fitness-function test-input
                                  &optional (size 100))
  (let ((population (make-instance 'population
                                   :fitness-function fitness-function
                                   :motes (make-array 0 :fill-pointer 0)
                                   :operators operators
                                   :size size
                                   :test-input test-input)))
    (loop with n-motes = 0
          until (>= n-motes size)
          for mote = (create-mote operators fitness-function test-input)
          when (fitness mote)
          do (vector-push-extend mote (motes population))
             (incf n-motes))
    (sort-motes population)
    population))


(defun create-mote (operators fitness-function input &key (debug nil))
  "Creates a new MOTE instance with a fitness guaranteed not be NIL."
  (loop for i from 1
        for rtree = (create-random-tree operators)
        for fitness =  (calculate-fitness rtree fitness-function input)
        until fitness
        finally (when debug
                  (format t "[create-mote] took ~A tries.~%" i))
                (return (make-instance 'mote :fitness fitness :tree rtree
                                       :n-nodes (calculate-n-nodes rtree)))))


(defun create-random-tree (operators &key (max-depth 4))
  "Generates a random tree using OPERATORS.  MAX-DEPTH must either be an
  integer equal to or greater than 0 or :UNLIMITED.  The latter is not
  recommended."
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (or (> max-depth 0) (equal max-depth :unlimited))
                            (if (= 0 (random 2))
                                (create-random-tree operators
                                                    :max-depth (- max-depth 1))
                                (if (= 0 (random 2))
                                    (random 10.0)
                                    '=input=))
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))