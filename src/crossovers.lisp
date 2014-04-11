;;;; crossovers.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions

(defun crossover (tree1 tree2 &key (both nil) (debug nil))
  "Returns a new tree similar to TREE1 but with a random node replaced by a
  random node from TREE2.  If BOTH is T another tree will be returns as
  second value similar to TREE2 but with a random node replace by a random
  node from TREE1."
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (when debug
      (format t "tree1: ~S~%tree2: ~S~%rnode1: ~S~%rnode2: ~S~%"
              tree1 tree2 rnode1 rnode2))
    (if both
        (values
         (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))
         (replace-node tree2 (getf rnode2 :index) (getf rnode1 :node)))
        (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node)))))
