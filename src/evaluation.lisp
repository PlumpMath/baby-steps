;;;; evaluation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions & Methods

(defmethod advance-generation ((p population) &key (method :tournament))
  (case method
    (:fitness-proportionate (advance-generation-fitness-proportionate p))
    (:tournament (advance-generation-tournament p))
    (otherwise (error "Unknown ADVANCE-GENERATION method: ~S" method)))
  (population-to-size p)
  (setf (diversity p) (population-diversity p))
  ;; While this has an effect, it just means the population now fills up with
  ;; not exact duplicates but almost exact duplicates (with the same fitness
  ;; no less!).
  (when (< (diversity p) 0.8)
    (replace-duplicates p)))


;; Not quite fitness-proportionate I think, need to check.
(defmethod advance-generation-fitness-proportionate ((p population))
  (normalise-fitness p)
  (let ((ffn (fitness-fn p))
        (ti (test-input p))
        (motes (head (sort (motes p) (lambda (a b)
                                       (> (fitness a) (fitness b))))
                     (size p))))
    (setf (fill-pointer (motes p)) 0)
    (loop for mote across motes
          do (vector-push-extend mote (motes p))
             (when (<= (random 1.0d0) (normalised-fitness mote))
               (let ((rnr (random 100)))
                 (cond ((<= rnr 80)
                        (let* ((tree (crossover (tree mote)
                                                (tree (random-elt motes))))
                               (new-mote (create-mote-from-tree tree ffn ti)))
                          (when (fitness new-mote)
                            (vector-push-extend new-mote (motes p)))))
                       ((<= rnr 90)
                        (let* ((tree (mutate (tree mote) (operators p)))
                               (new-mote (create-mote-from-tree tree ffn ti)))
                          (when (fitness new-mote)
                            (vector-push-extend new-mote (motes p)))))
                       (t
                        (let* ((tree (bloat-to-float (tree mote)))
                               (new-mote (create-mote-from-tree tree ffn ti)))
                          (when (fitness new-mote)
                            (vector-push-extend new-mote (motes p)))))))))))


(defmethod advance-generation-tournament ((p population))
  (let ((ffn (fitness-fn p))
        (ti (test-input p))
        (participants (select-tournament-participants p)))
    (loop while participants
          for p1 = (pop participants)
          for p2 = (pop participants)
          do (let ((rnr (random 100)))
               (cond ((<= rnr 80)
                      (let* ((tree (crossover (tree p1) (tree p2)))
                             (new-mote (create-mote-from-tree tree ffn ti)))
                        (when (fitness new-mote)
                          (vector-push-extend new-mote (motes p)))))
                     ((<= rnr 90)
                      (let* ((tree (mutate (tree p1) (operators p)))
                             (new-mote (create-mote-from-tree tree ffn ti)))
                        (when (fitness new-mote)
                          (vector-push-extend new-mote (motes p)))))
                     (t
                      (let* ((tree (bloat-to-float (tree p1)))
                             (new-mote (create-mote-from-tree tree ffn ti)))
                        (when (fitness new-mote)
                          (vector-push-extend new-mote (motes p))))))))))


(defun calculate-fitness (tree fitness-fn test-input &key (debug nil))
  (loop with fn = ;; TODO test whether handler-case is needed here
                  (handler-case (make-function tree)
                    (error () (return-from calculate-fitness nil)))
        with fs = nil
        for i in test-input
        ;for out = (handler-case (run-tree tree i)
        for out = (handler-case (funcall fn i)
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


(defmethod replace-duplicates ((p population))  ; needs :method
  "Replaces any duplicate in POPULATION by the result of CREATE-MOTE.
  (MOTES POPULATION) needs to be sorted using SORT-MOTES beforehand, otherwise
  this function will not return correct results."
  (loop with prev-tree = nil
        for mote across (motes p)
        for i from 0
        for tree = (tree mote)
        do (if (equal tree prev-tree)
               (setf (aref (motes p) i)
                     (create-mote (operators p) (fitness-fn p) (test-input p)))
               (setf prev-tree tree))))


;; Doesn't actually ever reach 0 since a P of 100 will have max 99 dups.
(defmethod population-diversity ((p population))
  "Returns the diversity of POPULATION as a float, where 1.0 means completely
  diverse (no duplicates) and 0.0 means all the motes in POPULATION are
  similar.
  (MOTES POPULATION) needs to be sorted using SORT-MOTES beforehand, otherwise
  this function will not return correct results."
  (loop with duplicates = 0
        with prev-tree = nil
        for mote across (motes p)
        for tree = (tree mote)
        do (when (equal tree prev-tree)
             (incf duplicates))
           (setf prev-tree tree)
        finally (return (- 1.0 (/ duplicates (size p))))))


(defmethod population-to-size ((p population))
  "Makes sure the number of motes in POPULATION (P) is equal to (SIZE P).
  If there's more motes than (SIZE P), (MOTES P) is first sorted using
  SORT-MOTES and then the fill-pointer is set to (SIZE P).
  If there's less motes, CREATE-MOTE is used until (SIZE P) is reached."
  (cond ((> (length (motes p)) (size p))
         (sort-motes p)
         (setf (fill-pointer (motes p)) (size p)))
        ((< (length (motes p)) (size p))
         (loop repeat (- (size p) (length (motes p)))
               do (vector-push-extend
                   (create-mote (operators p) (fitness-fn p) (test-input p))
                   (motes p)))
         (sort-motes p))))


(defmethod select-tournament-participants ((p population))
  (loop with candidate-indexes = nil
        with candidate-nr = 0
        with n-candidates = (if (<= (size p) 10)
                                2
                                (let ((n (floor (* 0.2 (size p)))))
                                  (if (oddp n) (+ n 1) n)))
        until (>= candidate-nr n-candidates)
        for rnr = (random (size p))
        do (unless (member rnr candidate-indexes)
             (push rnr candidate-indexes)
             (incf candidate-nr))
        finally (return (sort (loop for index in candidate-indexes
                                    collect (nth-mote p index))
                              (lambda (a b)
                                (> (fitness a) (fitness b)))))))


(defmethod sort-motes ((obj population))
  "Sorts (MOTES OBJ) from best fitness to worst fitness."
  (setf (motes obj)
        (sort (motes obj) (lambda (a b) (> (fitness a) (fitness b))))))
