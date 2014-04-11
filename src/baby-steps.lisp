;;;; baby-steps.lisp
;;;;
;;;; Notes to self:
;;;; o Would multiple crossovers / mutations per individual per run get some
;;;;   runs out of a local minimum?

(in-package :baby-steps)


;;; Functions & Methods

(defun nth-mote (population index)
  (elt (motes population) index))


(defun print-trees (population)
  (loop for mote across (motes population)
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


;; XXX seems a suboptimal approach
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
