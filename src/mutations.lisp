;;;; mutations.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions

;; Subtree mutation.
(defun mutate (tree operators &key (debug nil))
  "Replaces a random node in TREE with a random tree."
  (let ((rtree (create-random-tree operators))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rtree: ~S~%rnode: ~S~%" tree rtree rnode))
    (replace-node tree (getf rnode :index) rtree)))


;; This is apparently a subset of Shrink Mutation.
(defun bloat-to-float (tree &key (debug nil))  ; best function name ever?
  "Replaces a random node in TREE with a float."
  (let ((float (random 10.0))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rnode: ~S~%float: ~S~%" tree rnode float))
    (replace-node tree (getf rnode :index) float)))
