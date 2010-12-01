;;;; baby-steps.lisp
;;;;
;;;; Usage:
;;;; 1> (defparameter population (create-initial-population 1))
;;;; 2> (evaluate-population population *fitness-function* *input*)
;;;;
;;;; Notes to self:
;;;; * #'sb-introspect:function-lambda-list

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
  (append list (list object)))


(defun create-initial-population (&optional (size 10))
  (loop repeat size
        collect (random-lambda *operators*)))


(defun cross-over (tree1 tree2)
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (list
     :tree1 (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))
     :tree2 (replace-node tree2 (getf rnode2 :index) (getf rnode1 :node)))))


(defun evaluate-population (population fitness-function input)
  (loop for i from 0 below (length population)
        for lambda = (elt population i)
        do (format t "=== ~S ===~%" i)
           (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
           (format t "---------|--------------------------~
                               |--------------------------~%")
           (loop for j from 0 below (length input)
                 for in = (elt input j)
                 for out = (run-lambda lambda in)
                 for target = (funcall fitness-function in)
                 do (format t "~8@S | ~24@S | ~24@S~%" in out target))))


(defun mutate (tree operators)
  (let ((rform (random-form operators))
        (rnode (random-node tree)))
    (replace-node tree (getf rnode :index) rform)))


(defun n-nodes (tree)
  "Returns the number of nodes in TREE, including the leaves."
  (let ((nodes 1))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf nodes)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))
    nodes))


(defun random-elt (sequence)
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun random-form (operators)
  (append (list (random-elt operators))
          (loop repeat 2
                collect (if (= 0 (random 2))
                            (random-form operators)
                            (if (= 0 (random 2))
                                (random 10.0)
                                '=input=)))))


(defun random-lambda (operators)
  (append1 '(lambda (=input=))
           (random-form operators)))


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
  "Returns a new tree with SUBTREE in place of the old node at NODE-INDEX of
  TREE."
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


(defun run-lambda (lambda input)
  (funcall (eval lambda) input))
