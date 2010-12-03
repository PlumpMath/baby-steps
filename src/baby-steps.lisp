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


;;; Specials

(defparameter *fitness-function* (lambda (r) (* pi (* r r))))

(defparameter *input* '(1 23 456))

(defparameter *operators* '(+ - * /))


;;; Function

(defun append1 (list object)
  "Shorthand for: (APPEND LIST (LIST OBJECT))."
  (append list (list object)))


(defun create-initial-population (&optional (size 100))
  (loop repeat size
        collect (random-form *operators*)))


(defun cross-over (tree1 tree2 &key (debug nil))
  "Returns a new tree similar to TREE1 but with a random node replaced by a
  random node from TREE2."
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (when debug
      (format t "tree1: ~S~%tree2: ~S~%rnode1: ~S~%rnode2: ~S~%"
              tree1 tree2 rnode1 rnode2))
    (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))))


(defun evaluate-population (population fitness-function input &key (debug nil))
  "Returns POPULATION as a list consisting of (FITNESS FORM) and sorted by
  fitness from best to worst.  Any form that returned a NIL (ie. caused an
  error) is not included in the returned list."
  (when debug
    (format t "~8@A | ~24@A~%" "i" "fitness")
    (format t "---------|--------------------------~%"))
  (loop for i from 0 below (length population)
        for form = (elt population i)
        for fitness = (fitness form fitness-function input)
        when debug do (format t "~8@S | ~24@S~%" i fitness)
        unless (null fitness) collect (list fitness form) into result
        finally (return (sort result #'> :key #'first))))


(defun fitness (form fitness-function input &key (debug nil))
  (loop with fs = nil
        with max-f = 0
        for i in input
        for out = (handler-case (run-form form i)
                    (error () (return-from fitness nil)))
        for target = (funcall fitness-function i)
        for f = (abs (- target out))
        do (when (> f max-f)
             (setf max-f f))
           (push f fs)
           (when debug
             (format t "i=~S t=~S o=~S f=~S~%" i target out f))
        finally (return (if (= 0 max-f)
                            0
                            (loop with result = 1.0
                                  for f in fs
                                  do (setf result (* result (/ f max-f)))
                                  finally (return result))))))


(defun head (sequence &optional (amount 1))
  "Returns AMOUNT elements from the start of SEQUENCE.  If SEQUENCE is shorter
  than AMOUNT it will return SEQUENCE."
  (if (<= amount 0)
      nil
      (if (< (length sequence) amount)
          sequence
          (subseq sequence 0 amount))))


(defun make-function (form)
  "Turns FORM into a function object."
  (let ((*error-output* (make-broadcast-stream)))  ; thanks stassats!
    (eval (append1 '(lambda (=input=)) form))))


(defun mutate (tree operators &key (debug nil))
  "Replaces a random node in TREE with a random form."
  (let ((rform (random-form operators))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rform: ~S~%rnode: ~S~%" tree rform rnode))
    (replace-node tree (getf rnode :index) rform)))


(defun n-nodes (tree)
  "Returns the number of nodes in TREE, including the root node and leaves."
  (let ((nodes 1))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf nodes)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))
    nodes))


(defun print-results (form fitness-function input)
  (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
  (format t "---------|--------------------------~
                      |--------------------------~%")
  (loop for i from 0 below (length input)
        for in = (elt input i)
        for out = (run-form form in)
        for target = (funcall fitness-function in)
        do (format t "~8@S | ~24@S | ~24@S~%" in out target)))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun random-form (operators &key (max-depth 4))
  "Generates a random form using OPERATORS.  MAX-DEPTH must either be an
  integer equal to or greater than 0 or :UNLIMITED.  The latter is not
  recommended."
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (or (> max-depth 0) (equal max-depth :unlimited))
                            (if (= 0 (random 2))
                                (random-form operators
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
         (nodes-1 (- (n-nodes tree) 1))
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


(defun revitalize-population (evaluated-population &key (members 100))
  "EVALUATED-POPULATION is the output of EVALUATE-POPULATION."
  (declare (ignore members))
  (let ((culled-population (head evaluated-population 90)))  ; kill bottom 10%
    ;; duplicate the top 10%
    (loop for i from 0 below 10
          collect (elt culled-population i) into dups
          finally (setf culled-population (append culled-population dups)))
    ;; add new members
    (when (< (length culled-population) 100)
      (loop repeat (- 100 (length culled-population))
            do (setf culled-population
                     (append1 culled-population
                              (list 0 (random-form *operators*))))))
    (loop for digitanism in culled-population
          for form = (second digitanism)
          for i from 69 downto 0 by 0.5
          ;collect (second digitanism))))
          collect (if (> (random 100) i)
                      form
                      (if (= 0 (random 2))
                          (cross-over form
                                      (elt culled-population
                                          (random (length culled-population))))
                          (mutate form *operators*))))))


(defun run-form (form input)
  "Turns FORM into a function and calls it with INPUT."
  (funcall (make-function form) input))


(defun run-generations (population fitness-function input
                        &optional (generations 10))
  (loop repeat generations
        for i from 0
        for ep = (evaluate-population population fitness-function input)
        do (format t "--- ~S ---~%1: ~S~%2: ~S~%3: ~S~%members: ~S~%"
                   i (first (first ep)) (first (second ep)) (first (third ep))
                   (length ep))
           (setf population (revitalize-population ep)))
  population)


