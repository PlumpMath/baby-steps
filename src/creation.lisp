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
                 :tree (copy-tree (tree mote))))


(defun create-mote (operators terminals &key (debug nil) (max-depth 4)
                    (max-operator-arity 4) (method :grow))
  "Creates a new MOTE instance.  FITNESS will be NIL, it is the reponsability
  of the API user to calculate the FITNESS."
  (let ((rtree (create-random-tree operators terminals :method method
                 :max-depth max-depth :max-operator-arity max-operator-arity)))
    (create-mote-from-tree rtree)))


(defun create-mote-from-tree (tree)
  "Creates a new MOTE instance based on TREE."
  (make-instance 'mote :tree tree :n-nodes (calculate-n-nodes tree)))


(defun create-population (operators terminals &key (max-depth 4) (size 100)
                          (method :ramped-half-half))
  "METHOD must be :FULL, :GROW or :RAMPED-HALF-HALF.
  CREATE-INITIAL-POPULATION does not check if the population has invalid
  members (ie. members which will not pass your fitness function).  Either
  use your own population creation function or replace invalid members
  afterwards."
  (let ((population (make-instance 'population :operators operators :size size
                                   :motes (make-array size :adjustable t
                                                      :fill-pointer 0)
                                   :terminals terminals)))
    (loop with n-motes = 0
          until (>= n-motes size)
          ;; always ramp up to max-depth from 0
          for cm-max-depth = (ceiling (* max-depth (/ n-motes size)))
          for cm-method = (if (equal method :ramped-half-half)
                              (if (evenp n-motes) :full :grow)
                              method)
          for mote = (create-mote operators terminals :max-depth cm-max-depth
                                  :method cm-method)
          do (add-to-population population mote)
             (incf n-motes))
    population))


(defun create-random-tree (operators terminals &key (max-depth 4)
                           (max-operator-arity 4) (method :grow))
  "Calls either CREATE-RANDOM-TREE-FULL or CREATE-RANDOM-TREE-GROW depending
  on METHOD.
  METHOD must be either :FULL or :GROW (default)."
  (case method
    (:full (create-random-tree-full operators terminals :max-depth max-depth
                                    :max-operator-arity max-operator-arity))
    (:grow (create-random-tree-grow operators terminals :max-depth max-depth
                                    :max-operator-arity max-operator-arity))))


(defun create-random-tree-full (operators terminals &key (max-depth 4)
                                (max-operator-arity 4))
  "Generates a random tree using OPERATORS and TERMINALS.  Will only generate
  OPERATORS until MAX-DEPTH is 0 at which point this function will only
  generate TERMINALS.
  MAX-DEPTH must be an integer equal to or greater than 0."
  (let ((op (random-elt operators)))
    (append (list (getf op :operator))
            (loop with arity = (getf op :arity)
                  with type = (getf op :type)
                  with ters = (loop for ter in terminals
                                    when (subtypep (getf ter :type) type)
                                      collect (getf ter :terminal))
                  repeat (if (equal arity :max)
                             (+ (random max-operator-arity) 1)
                             arity)
                  collect (if (> max-depth 0)
                              (create-random-tree-full operators terminals
                                        :max-depth (- max-depth 1)
                                        :max-operator-arity max-operator-arity)
                              (terminal-value (random-elt ters)))))))


(defun create-random-tree-grow (operators terminals &key (max-depth 4)
                                (max-operator-arity 4))
  "Generates a random tree using OPERATORS and TERMINALS.
  MAX-DEPTH must be an integer equal to or greater than 0."
  (let ((op (random-elt operators)))
    (append (list (getf op :operator))
            (loop with arity = (getf op :arity)
                  with type = (getf op :type)
                  with ters = (loop for ter in terminals
                                    when (subtypep (getf ter :type) type)
                                      collect (getf ter :terminal))
                  repeat (if (equal arity :max)
                             (+ (random max-operator-arity) 1)
                             arity)
                  collect (if (> max-depth 0)
                              (if (= 0 (random 2))  ; (random 2) returns 0 or 1
                                  (create-random-tree-grow operators terminals
                                        :max-depth (- max-depth 1)
                                        :max-operator-arity max-operator-arity)
                                  (terminal-value (random-elt ters)))
                              (terminal-value (random-elt ters)))))))


(defun terminal-value (terminal)
  "Returns the value of TERMINAL.
  Terminals are defined as such:

    (defparameter *terminals* `((:terminal =input= :type number :input t)
                                (:terminal ,#'random-int-10 :type number)
                                (:terminal ,#'random-float-10 :type number)))

  TERMINAL must either be a plist or a (GETF plist :TERMINAL)."
  (let ((ter (if (listp terminal)
                 (getf terminal :terminal)
                 terminal)))
    (if (typep ter 'function)
        (funcall ter)
        ter)))
