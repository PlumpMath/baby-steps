;;;; evaluation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions & Methods

(defmethod add-best-mote ((p population) (m mote))
  "Adds MOTE to (BEST-MOTES POPULATION) if it isn't already in there."
  (when (find m (best-motes p))
    (return-from add-best-mote (best-motes p)))
  (if (< (length (best-motes p)) (best-size p))
      (vector-push-extend m (best-motes p))
      (setf (elt (best-motes p) (- (best-size p) 1)) m))
  (sort-best-motes p))


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


(defmethod sort-best-motes ((obj population))
  "Sorts (BEST-MOTES OBJ) from best fitness to worst fitness."
  (setf (best-motes obj)
        (sort (best-motes obj) (lambda (a b) (> (fitness a) (fitness b))))))


(defmethod sort-motes ((obj population))
  "Sorts (MOTES OBJ) from best fitness to worst fitness."
  (setf (motes obj)
        (sort (motes obj) (lambda (a b) (> (fitness a) (fitness b))))))
