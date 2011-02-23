;;;; evaluation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions & Methods

;; Not quite fitness-proportionate I think, need to check.
(defmethod advance-generation-fitness-proportionate ((p population))
  (normalise-fitness p)
  (let ((ffn (fitness-fn p))
        (ti (test-input p))
        (motes-copy (head (sort (motes p) (lambda (a b)
                                            (> (fitness a) (fitness b))))
                          (size p))))
    (setf (fill-pointer (motes p)) 0)
    (loop for mote across motes-copy
          for i from 0
          do (vector-push-extend mote (motes p))
             (when (<= (random 1.0d0) (normalised-fitness mote))
               (if (<= (random 100) 90)
                   (let* ((tree (crossover (tree mote)
                                           (tree (random-elt motes-copy))))
                          (fitness (calculate-fitness tree ffn ti)))
                     (when fitness
                       (vector-push-extend
                        (make-instance 'mote :tree tree :fitness fitness
                                       :fn (make-function tree)
                                       :n-nodes (calculate-n-nodes tree))
                        (motes p))))
                   (let* ((tree (mutate (tree mote) (operators p)))
                          (fitness (calculate-fitness tree ffn ti)))
                     (when fitness
                       (vector-push-extend
                        (make-instance 'mote :tree tree :fitness fitness
                                       :fn (make-function tree)
                                       :n-nodes (calculate-n-nodes tree))
                        (motes p)))))))
    (cond ((> (length (motes p)) (size p))
           (sort-motes p)
           (setf (fill-pointer (motes p)) (size p)))
          ((< (length (motes p)) (size p))
           (loop repeat (- (size p) (length (motes p)))
                 do (vector-push-extend (create-mote (operators p)
                                                 (fitness-fn p) (test-input p))
                                        (motes p)))
           (sort-motes p)))))


(defmethod advance-generation-tournament ((p population))
  (let ((ffn (fitness-fn p))
        (ti (test-input p))
        (participants
         (loop with candidate-indexes = nil
               with candidate-nr = 0
               with n-candidates = 20
               until (>= candidate-nr n-candidates)
               for rnr = (random (size p))
               do (unless (member rnr candidate-indexes)
                    (push rnr candidate-indexes)
                    (incf candidate-nr))
               finally (return (sort (loop for index in candidate-indexes
                                           collect (nth-mote p index))
                                     (lambda (a b)
                                       (> (fitness a) (fitness b))))))))
    (loop with i = 0
          until (>= i (length participants))
          for p1 = (elt participants i)
          for p2 = (elt participants (+ i 1))
          do (incf i 2)
             (if (<= (random 100) 90)
                 (let* ((tree (crossover (tree p1) (tree p2)))
                        (fitness (calculate-fitness tree ffn ti)))
                   (when fitness
                     (vector-push-extend
                      (make-instance 'mote :tree tree :fitness fitness
                                     :fn (make-function tree)
                                     :n-nodes (calculate-n-nodes tree))
                      (motes p))))
                 (let* ((tree1 (mutate (tree p1) (operators p)))
                        (fitness1 (calculate-fitness tree1 ffn ti))
                        (tree2 (mutate (tree p2) (operators p)))
                        (fitness2 (calculate-fitness tree2 ffn ti)))
                     (when fitness1
                       (vector-push-extend
                        (make-instance 'mote :tree tree1 :fitness fitness1
                                       :fn (make-function tree1)
                                       :n-nodes (calculate-n-nodes tree1))
                        (motes p)))
                     (when fitness2
                       (vector-push-extend
                        (make-instance 'mote :tree tree2 :fitness fitness2
                                       :fn (make-function tree2)
                                       :n-nodes (calculate-n-nodes tree2))
                        (motes p))))))
    (cond ((> (length (motes p)) (size p))
           (sort-motes p)
           (setf (fill-pointer (motes p)) (size p)))
          ((< (length (motes p)) (size p))
           (loop repeat (- (size p) (length (motes p)))
                 do (vector-push-extend (create-mote (operators p)
                                                 (fitness-fn p) (test-input p))
                                        (motes p)))
           (sort-motes p)))))


;(defmethod advance-generation ((p population) &key (method :tournament))
(defmethod advance-generation ((p population)
                               &key (method :fitness-proportionate))
  (case method
    (:fitness-proportionate (advance-generation-fitness-proportionate p))
    (:tournament (advance-generation-tournament p))
    (otherwise (error "Unknown ADVANCE-GENERATION method."))))


(defun calculate-fitness (tree fitness-fn test-input &key (debug nil))
  (loop with fs = nil
        for i in test-input
        for out = (handler-case (run-tree tree i)
                    (error () (return-from calculate-fitness nil)))
        for target = (funcall fitness-fn i)
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


(defmethod sort-motes ((obj population))
  "Sorts (MOTES OBJ) from best fitness to worst fitness."
  (setf (motes obj)
        (sort (motes obj) (lambda (a b) (> (fitness a) (fitness b))))))
