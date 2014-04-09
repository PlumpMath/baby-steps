;;;; baby-steps.lisp
;;;;
;;;; Notes to self:
;;;; o Would multiple crossovers / mutations per individual per run get some
;;;;   runs out of a local minimum?

(in-package :baby-steps)


;;; Functions & Methods

(defmethod nth-mote ((p population) index)
  (elt (motes p) index))


(defmethod print-trees ((p population))
  (loop for mote across (motes p)
        for i from 0
        do (format t "--- ~D ---~%~S~%" i (tree mote))))


(defun random-node (tree)
  "Returns a random node from TREE.
  Format '(:INDEX index-of-node :NODE the-actual-node)."
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


(defun run-tree (tree terminals input)
  "Turns TREE into a function and calls it with INPUT."
  (funcall (make-function tree terminals) input))
