;;;; mutations.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions

;; Subtree mutation.
(defun mutate (tree operators terminals &key (debug nil))
  "Replaces a random node in TREE with a random tree."
  (let ((rtree (create-random-tree operators terminals))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rtree: ~S~%rnode: ~S~%" tree rtree rnode))
    (replace-node tree (getf rnode :index) rtree)))
