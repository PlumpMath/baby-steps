;;;; baby-steps.lisp
;;;;
;;;; Usage:
;;;; 1> (defparameter popl (create-initial-population 1))
;;;; 2> (setf popl (run-generations population *operators* *fitness-function*
;;;;                                *input*))
;;;;
;;;; Notes to self:
;;;; * #'sb-introspect:function-lambda-list
;;;; * swank possibly provides 'trivial-introspect'

;;; Packages

(defpackage :baby-steps
  (:use :cl))

(in-package :baby-steps)


;;; Classes

;; N-NODES can be deduced but I use it in PRINT-OBJECT and don't want to
;; recalculate every time I print objects to the REPL.
(defclass subtree ()
  ((n-nodes :accessor n-nodes :initarg :n-nodes)
   (tree :accessor tree :initarg :tree)))


;; FITNESS can be deduced but I use it in PRINT-OBJECT and don't want to
;; recalculate every time I print objects to the REPL.
(defclass mote (subtree)
  ((fitness :accessor fitness :initarg :fitness)))


(defclass population ()
  ((best-motes :accessor best-motes :initform (make-array 0 :fill-pointer 0))
   (best-size :accessor best-size :initarg :best-size :initform 10)
   (fitness-function :accessor fitness-function :initarg :fitness-function)
   (motes :accessor motes :initarg :motes)
   (operators :accessor operators :initarg :operators)
   (size :accessor size :initarg :size :initform 100)
   (test-input :accessor test-input :initarg :test-input :initform '(-1 0 1))))


(defmethod print-object ((obj mote) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness=~A n-nodes=~A" (fitness obj) (n-nodes obj))))


(defmethod print-object ((obj population) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "size=~A" (size obj))))


;;; Specials

(defparameter *fitness-function* (lambda (r) (* pi (* r r))))

(defparameter *input* '(-1 1 23 456))

(defparameter *operators* '(+ - * /))


;;; Functions & Methods

(defmethod add-best-mote ((p population) (m mote))
  "Adds MOTE to (BEST-MOTES POPULATION) if it isn't already in there."
  (when (find m (best-motes p))
    (return-from add-best-mote (best-motes p)))
  (if (< (length (best-motes p)) (best-size p))
      (vector-push-extend m (best-motes p))
      (setf (elt (best-motes p) (- (best-size p) 1)) m))
  (sort-best-motes p))


(defun append1 (list object)
  "Shorthand for: (APPEND LIST (LIST OBJECT))."
  (append list (list object)))


;; XXX: doesn't work quite right yet, the last mote isn't correct
(defmethod best-mote-p ((p population) (m mote))
  (if (< (length (best-motes p)) (best-size p))
      t
      (let ((last-best (elt (best-motes p) (- (best-size p) 1))))
        (if (> (fitness m) (fitness last-best))
            t
            ;; fitness is equal, select on size or newness:
            (<= (n-nodes m) (n-nodes last-best))))))


(defun calculate-fitness (tree fitness-function input &key (debug nil))
  (loop with fs = nil
        for i in input
        for out = (handler-case (run-tree tree i)
                    (error () (return-from calculate-fitness nil)))
        for target = (funcall fitness-function i)
        for dto = (abs (- target out))
        for f = (/ 1.0 (+ 1 dto))
        do (push f fs)
           (when debug
             (format t "i=~S t=~S o=~S f=~S~%" i target out f))
        finally (return (reduce #'* fs))))


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
        for rtree = (random-tree operators)
        for fitness =  (calculate-fitness rtree fitness-function input)
        until fitness
        finally (when debug
                  (format t "[create-mote] took ~A tries.~%" i))
                (return (make-instance 'mote :fitness fitness :tree rtree
                                       :n-nodes (calculate-n-nodes rtree)))))


(defun cross-over (tree1 tree2 &key (debug nil))
  "Returns a new tree similar to TREE1 but with a random node replaced by a
  random node from TREE2."
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (when debug
      (format t "tree1: ~S~%tree2: ~S~%rnode1: ~S~%rnode2: ~S~%"
              tree1 tree2 rnode1 rnode2))
    (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))))


(defmethod evaluate-population ((obj population))
  "Recalculates the fitness of every mote in POPULATION and sorts the motes
  using #'SORT-MOTES.  Any mote that returned a NIL for fitness (ie. caused an
  error) is replaced by a newly created mote."
  (loop for i from 0 below (size obj)
        for mote = (elt (motes obj) i)
        for fitness = (calculate-fitness (tree mote) (fitness-function obj)
                                         (test-input obj))
        when fitness
          do (setf (fitness mote) fitness)
             ;; XXX: doesn't work quite right yet, the last mote isn't correct
             (when (best-mote-p obj mote)
               (add-best-mote obj mote))
        unless fitness
          do (setf (elt (motes obj) i)
                   (create-mote (operators obj) (fitness-function obj)
                                (test-input obj)))
        finally (return (sort-motes obj))))


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


(defmethod revitalize-population ((p population))
  ;; kill bottom 20%
  (setf (fill-pointer (motes p)) (floor (* 0.8 (length (motes p)))))
  ;; cross-overs and mutations
  (loop for mote across (motes p)
        for rnr = (random 100)
        for cross-over = (when (<= rnr 90)
                           (cross-over (tree mote)
                                       (tree (elt (motes p)
                                                (random (length (motes p)))))))
        for mutate = (unless cross-over
                       (mutate (tree mote) (operators p)))
        for tree = (if cross-over cross-over mutate)
        for fitness = (calculate-fitness tree (fitness-function p)
                                         (test-input p))
        when fitness
        do (vector-push-extend (make-instance 'mote :fitness fitness :tree tree
                                             :n-nodes (calculate-n-nodes tree))
                               (motes p)))
  ;; add new members
  (when (< (length (motes p)) (size p))
    (loop repeat (- (size p) (length (motes p)))
          do (vector-push-extend (create-mote (operators p)
                                        (fitness-function p) (test-input p))
                                 (motes p))))
  (evaluate-population p)
  (when (> (fill-pointer (motes p)) (size p))
    (setf (fill-pointer (motes p)) (size p)))
  (motes p))


(defun run-tree (tree input)
  "Turns TREE into a function and calls it with INPUT."
  (funcall (make-function tree) input))


(defun run-generations (population &optional (generations 10))
  (loop repeat generations
        for i from 0
        for lm = (length (motes population))
        do (revitalize-population p)
           ;(when (<= (fitness (elt ep 0)) 0)
           ;  (format t "!!! Solution Found !!!~%")
           ;  (return-from run-generations ep))
           (format t "[~S] ~15,10E ~15,10E ~15,10E ~15,10E~%" i
                   (fitness (elt (motes population) 0))
                   (fitness (elt (motes population) (floor (* 0.1 lm))))
                   (fitness (elt (motes population) (floor (* 0.5 lm))))
                   (fitness (elt (motes population) (- lm 1)))))
  population)


(defmethod sort-best-motes ((obj population))
  "Sorts (BEST-MOTES OBJ) from best fitness to worst fitness."
  (setf (best-motes obj)
        (sort (best-motes obj) (lambda (a b) (> (fitness a) (fitness b))))))


(defmethod sort-motes ((obj population))
  "Sorts (MOTES OBJ) from best fitness to worst fitness."
  (setf (motes obj)
        (sort (motes obj) (lambda (a b) (> (fitness a) (fitness b))))))
