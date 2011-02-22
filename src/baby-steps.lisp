;;;; baby-steps.lisp
;;;;
;;;; Notes to self:
;;;; * #'sb-introspect:function-lambda-list
;;;; * swank possibly provides 'trivial-introspect'

(in-package :baby-steps)


;;; Functions & Methods

;; This depends on the example :-|
(defun make-function (tree)
  "Turns TREE into a function object."
  (let ((*error-output* (make-broadcast-stream)))  ; thanks stassats!
    (eval (append1 '(lambda (=input=)) tree))))


;(defun print-results (tree fitness-function input)
;  (format t "~8@A | ~24@A | ~24@A~%" "in" "out" "wanted")
;  (format t "---------|--------------------------~
;                      |--------------------------~%")
;  (loop for i from 0 below (length input)
;        for in = (elt input i)
;        for out = (run-tree tree in)
;        for target = (funcall fitness-function in)
;        do (format t "~8@S | ~24@S | ~24@S~%" in out target)))


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


(defun run-tree (tree input)
  "Turns TREE into a function and calls it with INPUT."
  (funcall (make-function tree) input))
