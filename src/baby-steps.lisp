;;;; baby-steps.lisp
;;;;
;;;; Usage:
;;;; 1> (defparameter population (create-initial-population 1))
;;;; 2> (evaluate-population population *fitness-function* *input*)
;;;;
;;;; Notes to self:
;;;; * #'sb-introspect:function-lambda-list
;;;; * swank possibly provides 'trivial-introspect'

;;; Packages

(defpackage :baby-steps
  (:use :cl))

(in-package :baby-steps)


;;; Classes

;; FITNESS and N-NODES can be deduced but I use them in PRINT-OBJECT and don't
;; want to recalculate them every time I print objects to the REPL.
(defclass mote ()
  ((fitness :accessor fitness :initarg :fitness)
   (n-nodes :accessor n-nodes :initarg :n-nodes)
   (tree :accessor tree :initarg :tree)))


(defmethod print-object ((obj mote) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness=~A n-nodes=~A" (fitness obj) (n-nodes obj))))


;;; Specials

(defparameter *best-mote* nil)

(defparameter *fitness-function* (lambda (r) (* pi (* r r))))

(defparameter *input* '(-1 1 23 456))

(defparameter *operators* '(+ - * /))


;;; Function

(defun append1 (list object)
  "Shorthand for: (APPEND LIST (LIST OBJECT))."
  (append list (list object)))


(defun calculate-fitness (tree fitness-function input &key (debug nil))
  (loop with fs = nil
        for i in input
        for out = (handler-case (run-tree tree i)
                    (error () (return-from calculate-fitness nil)))
        for target = (funcall fitness-function i)
        for f = (abs (- 1 (/ out target)))  ; XXX: possible divide-by-zero
        do (push f fs)
           (when debug
             (format t "i=~S t=~S o=~S f=~S~%" i target out f))
        finally (return (coerce (/ (reduce #'+ fs) (length fs)) 'float))))


(defun calculate-n-nodes (tree)
  "Returns the number of nodes in TREE, including the root node and leaves."
  (let ((nodes 1))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf nodes)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))
    nodes))


(defun copy-mote (mote)
  (make-instance 'mote :fitness (fitness mote) :n-nodes (n-nodes mote)
                 :tree (tree mote)))


(defun create-initial-population (operators fitness-function input
                                  &optional (size 100))
  (loop repeat size
        for rtree = (random-tree operators)
        for fitness = (calculate-fitness rtree fitness-function input)
        for n-nodes = (calculate-n-nodes rtree)
        collect (create-mote operators fitness-function input)))


(defun create-mote (operators fitness-function input)
  (let ((rtree (random-tree operators)))
    (make-instance 'mote
                   :fitness (calculate-fitness rtree fitness-function input)
                   :n-nodes (calculate-n-nodes rtree)
                   :tree rtree)))


(defun cross-over (tree1 tree2 &key (debug nil))
  "Returns a new tree similar to TREE1 but with a random node replaced by a
  random node from TREE2."
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (when debug
      (format t "tree1: ~S~%tree2: ~S~%rnode1: ~S~%rnode2: ~S~%"
              tree1 tree2 rnode1 rnode2))
    (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))))


(defun evaluate-population (population fitness-function input)
  "Recalculates the fitness of every mote in POPULATION and returns the
  population sorted by fitness from best to worst.  Any mote that returned a
  NIL for fitness (ie. caused an error) is not included in the returned list."
  (loop for i from 0 below (length population)
        for mote = (elt population i)
        for fitness = (calculate-fitness (tree mote) fitness-function input)
        when fitness
          do (setf (fitness mote) fitness)
             (unless *best-mote*
               (setf *best-mote* (copy-mote mote)))
             (when (< fitness (fitness *best-mote*))
               (setf *best-mote* (copy-mote mote)))
          and collect mote into result
        finally (return (sort result #'< :key #'fitness))))


(defun head (sequence &optional (amount 1))
  "Returns AMOUNT elements from the start of SEQUENCE.  If SEQUENCE is shorter
  than AMOUNT it will return SEQUENCE."
  (if (<= amount 0)
      nil
      (if (< (length sequence) amount)
          sequence
          (subseq sequence 0 amount))))


(defun make-function (tree)
  "Turns TREE into a function object."
  (let ((*error-output* (make-broadcast-stream)))  ; thanks stassats!
    (eval (append1 '(lambda (=input=)) tree))))


(defun mutate (tree operators &key (debug nil))
  "Replaces a random node in TREE with a random tree."
  (let ((rtree (random-tree operators))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rtree: ~S~%rnode: ~S~%" tree rtree rnode))
    (replace-node tree (getf rnode :index) rtree)))


(defun print-results (tree fitness-function input)
  (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
  (format t "---------|--------------------------~
                      |--------------------------~%")
  (loop for i from 0 below (length input)
        for in = (elt input i)
        for out = (run-tree tree in)
        for target = (funcall fitness-function in)
        do (format t "~8@S | ~24@S | ~24@S~%" in out target)))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun random-tree (operators &key (max-depth 4))
  "Generates a random tree using OPERATORS.  MAX-DEPTH must either be an
  integer equal to or greater than 0 or :UNLIMITED.  The latter is not
  recommended."
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (or (> max-depth 0) (equal max-depth :unlimited))
                            (if (= 0 (random 2))
                                (random-tree operators
                                             :max-depth (- max-depth 1))
                                (if (= 0 (random 2))
                                    (random 10.0)
                                    '=input=))
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))


(defun random-node (tree)
  "Returns a random node from TREE."
  (let* ((index 1)
         (nodes-1 (- (calculate-n-nodes tree) 1))
         (random-node (+ (random nodes-1) 1)))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (when (= index random-node)
                          (return-from random-node (list :index index
                                                         :node node)))
                        (incf index)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))))


(defun replace-node (tree node-index new-node)
  "Returns a new tree with NEW-NODE at NODE-INDEX of TREE."
  (let ((index 0))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf index)
                     when (= index node-index)
                       collect new-node
                     when (and (/= index node-index)
                               (not (listp node)))
                       collect node
                     when (and (/= index node-index)
                               (listp node))
                       collect (traverse-nodes node))))
      (traverse-nodes tree))))


(defun revitalize-population (population operators fitness-function input
                              &key (members 100))
  "POPULATION is assumed to be sorted from best to worst (ie. the output of
  EVALUATE-POPULATION)."
  (declare (ignore members))
  (let ((culled-population (head population 80)))  ; kill bottom 20%
    ;; duplicate the top 10%
    (loop for i from 0 below 10
          collect (copy-mote (elt culled-population i)) into dups
          finally (setf culled-population (append culled-population dups)))
    ;; add new members
    (when (< (length culled-population) 100)
      (loop repeat (- 100 (length culled-population))
            do (setf culled-population
                     (append1 culled-population
                             (create-mote operators fitness-function input)))))
    (loop for mote in culled-population
          for i from 69 downto 0 by 0.5
          do (when (< (random 100) i)
               (if (= 0 (random 2))
                   (setf (tree mote)
                         (cross-over (tree mote)
                                     (tree (elt culled-population
                                        (random (length culled-population))))))
                   (setf (tree mote) (mutate (tree mote) operators)))))
    culled-population))


(defun run-tree (tree input)
  "Turns TREE into a function and calls it with INPUT."
  (funcall (make-function tree) input))


(defun run-generations (population operators fitness-function input
                        &optional (generations 10))
  (loop repeat generations
        for i from 0
        for ep = (evaluate-population population fitness-function input)
        do (when (<= (fitness (elt ep 0)) 0)
             (format t "!!! Solution Found !!!~%")
             (return-from run-generations ep))
           (format t "[~S] 1=~S 2=~S 3=~S (members: ~S)~%" i
                   (fitness (elt ep 0)) (fitness (elt ep 1))
                   (fitness (elt ep 2)) (length ep))
           (setf population (revitalize-population ep operators
                                                   fitness-function input)))
  population)
