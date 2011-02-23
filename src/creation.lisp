;;;; creation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions

(defun copy-mote (mote)
  "Returns a new MOTE instance with the same slot values as MOTE."
  (make-instance 'mote :fitness (fitness mote) :fn (fn mote)
                       :n-nodes (n-nodes mote) :tree (copy-tree (tree mote))))


(defun create-initial-population (operators fitness-fn test-input
                                  &key (max-depth 4) (method :ramped-half-half)
                                       (size 100))
  (let ((population (make-instance 'population
                                   :fitness-fn fitness-fn
                                   :motes (make-array 0 :fill-pointer 0)
                                   :operators operators
                                   :size size
                                   :test-input test-input)))
    (loop with n-motes = 0
          until (>= n-motes size)
          ;; always ramp up to max-depth from 0
          for cm-max-depth = (ceiling (* max-depth (/ n-motes size)))
          for cm-method = (if (equal method :ramped-half-half)
                              (if (evenp n-motes) :full :grow)
                              method)
          for mote = (create-mote operators fitness-fn test-input
                                  :max-depth cm-max-depth :method cm-method)
          when (fitness mote)
          do (vector-push-extend mote (motes population))
             (incf n-motes))
    (sort-motes population)
    population))


(defun create-mote (operators fitness-fn test-input
                    &key (debug nil) (max-depth 4) (method :grow))
  "Creates a new MOTE instance with a fitness guaranteed not be NIL."
  (loop for i from 1
        for rtree = (create-random-tree operators :max-depth max-depth
                                        :method method)
        for fitness = (calculate-fitness rtree fitness-fn test-input)
        until fitness
        finally (when debug
                  (format t "[create-mote] took ~A tries.~%" i))
                (return (make-instance 'mote :fitness fitness
                                             :fn (make-function rtree)
                                             :n-nodes (calculate-n-nodes rtree)
                                             :tree rtree))))


(defun create-random-tree (operators &key (max-depth 4) (method :grow))
  "Generates a random tree using OPERATORS.  MAX-DEPTH must either be an
  integer equal to or greater than 0 or :UNLIMITED.  The latter is not
  recommended and only possible for the :GROW method.
  METHOD must be eithet :FULL or :GROW."
  (case method
    (:full (create-random-tree-full operators :max-depth max-depth))
    (:grow (create-random-tree-grow operators :max-depth max-depth))))


(defun create-random-tree-full (operators &key (max-depth 4))
  "Generates a random tree using OPERATORS.  MAX-DEPTH must be an integer equal
  to or greater than 0."
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (> max-depth 0)
                            (create-random-tree-full operators
                                                    :max-depth (- max-depth 1))
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))


(defun create-random-tree-grow (operators &key (max-depth 4))
  "Generates a random tree using OPERATORS.  MAX-DEPTH must either be an
  integer equal to or greater than 0 or :UNLIMITED.  The latter is not
  recommended."
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (or (> max-depth 0) (equal max-depth :unlimited))
                            (if (= 0 (random 2))
                                (create-random-tree-grow operators
                                                    :max-depth (- max-depth 1))
                                (if (= 0 (random 2))
                                    (random 10.0)
                                    '=input=))
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))
