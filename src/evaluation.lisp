;;;; evaluation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions & Methods

(defmethod advance-generation ((population population)
                               &key (method :tournament))
  (case method
    (:fitness-proportionate (advance-generation-fitness-proportionate
                             population))
    (:tournament (advance-generation-tournament population))
    (otherwise (error "Unknown ADVANCE-GENERATION method: ~S" method)))
  (population-to-size population)
  (setf (diversity population) (population-diversity population)))


(defmethod add-to-population ((population population) (m mote))
  (vector-push-extend m (motes population)))


(defmethod clear-motes ((population population))
  (setf (fill-pointer (motes population)) 0))


;; Not quite fitness-proportionate I think, need to check.
(defmethod advance-generation-fitness-proportionate ((p population))
  (normalise-fitness p)
  (let ((ffn (fitness-fn p))
        (ti (test-input p))
        (ters (terminals p))
        (motes (head (sort (motes p) (lambda (a b)
                                       (> (fitness a) (fitness b))))
                     (size p))))
    (clear-motes p)
    (loop for mote across motes
          do (add-to-population p mote)
             (when (<= (random 1.0d0) (normalised-fitness mote))
               (let ((random-nr (random 100)))
                 (cond ((<= random-nr 80)
                        (let* ((tree (crossover (tree mote)
                                                (tree (random-elt motes))))
                               (new-mote (create-mote-from-tree tree ters ffn
                                                                ti)))
                          (when (fitness new-mote)
                            (add-to-population p new-mote))))
                       ((<= random-nr 90)
                        (let* ((tree (mutate (tree mote) (operators p)
                                             (terminals p)))
                               (new-mote (create-mote-from-tree tree ters ffn
                                                                ti)))
                          (when (fitness new-mote)
                            (add-to-population p new-mote))))
                       (t
                        (let* ((tree (bloat-to-float (tree mote)))
                               (new-mote (create-mote-from-tree tree ters ffn
                                                                ti)))
                          (when (fitness new-mote)
                            (add-to-population p new-mote))))))))))


(defmethod advance-generation-tournament ((p population))
  (let ((participants (select-tournament-participants p)))
    (loop while participants
          for p1 = (pop participants)
          for p2 = (pop participants)
          do (let ((random-nr (random 100)))
               (if (<= random-nr 80)  ; FIXME hardcoded
                   (let* ((tree (crossover (tree p1) (tree p2)))
                          (new-mote (create-mote-from-tree tree)))
                     (add-to-population p new-mote))
                   (let* ((tree (mutate (tree p1) (operators p) (terminals p)))
                          (new-mote (create-mote-from-tree tree)))
                     (add-to-population p new-mote)))))))


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


(defun compare-fitness (mote-a mote-b)
  ;; Put motes with fitness NIL at the top since they might be newly created.
  (cond ((null (fitness mote-a)) t)
        ((null (fitness mote-b)) nil)
        (t (> (fitness mote-a) (fitness mote-b)))))


(defun make-function (tree terminals)
  "Turns TREE into a function object."
  (let ((*error-output* (make-broadcast-stream))  ; silence *error-output*
        (input-args (loop for terminal in terminals
                          when (getf terminal :input)
                            collect (getf terminal :terminal))))
    (eval (append1 `(lambda ,input-args) tree))))


(defmethod normalise-fitness ((population population))
  (let* ((min-max (loop for mote across (motes population)
                        maximize (fitness mote) into max-result
                        minimize (fitness mote) into min-result
                        finally (return (list :max max-result
                                              :min min-result))))
         (max (getf min-max :max))
         (min (getf min-max :min)))
    (loop for mote across (motes population)
          do (setf (normalised-fitness mote)
                   (if (= min max)
                       1.0
                       (/ (- (fitness mote) min) max))))))


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
                     (create-mote (operators p) (terminals p) (fitness-fn p)
                                  (test-input p)))
               (setf prev-tree tree))))


;; Doesn't actually ever reach 0 since a P of 100 will have max 99 dups.
(defmethod population-diversity ((population population))
  "Returns the diversity of POPULATION as a float, where 1.0 means completely
  diverse (no duplicates) and 0.0 means all the motes in POPULATION are
  similar.
  (MOTES POPULATION) needs to be sorted using SORT-MOTES beforehand, otherwise
  this function will not return correct results."
  (loop with duplicates = 0
        with prev-tree = nil
        for mote across (motes population)
        for tree = (tree mote)
        do (when (equal tree prev-tree)
             (incf duplicates))
           (setf prev-tree tree)
        finally (return (- 1.0 (/ duplicates (size population))))))


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
                   (create-mote (operators p) (terminals p) (fitness-fn p)
                                (test-input p))
                   (motes p)))
         (sort-motes p))))


;(defmethod select-tournament-participants ((p population))
;  (loop with candidate-indexes = nil
;        with candidate-nr = 0
;        with n-candidates = (if (<= (size p) 10)
;                                2
;                                (let ((n (floor (* 0.2 (size p)))))
;                                  (if (oddp n) (+ n 1) n)))
;        until (>= candidate-nr n-candidates)
;        for random-nr = (random (size p))
;        do (unless (member random-nr candidate-indexes)
;             (push random-nr candidate-indexes)
;             (incf candidate-nr))
;        finally (return (sort (loop for index in candidate-indexes
;                                    collect (nth-mote p index))
;                              #'compare-fitness))))
(defmethod select-tournament-participants ((p population))
  (loop with candidate-indexes = nil
        with candidate-nr = 0
        until (>= candidate-nr (size p))
        for random-nr = (random (size p))
        do (push random-nr candidate-indexes)
           (incf candidate-nr)
        finally (return (loop for index in candidate-indexes
                              collect (nth-mote p index)))))



(defmethod sort-motes ((population population))
  "Sorts (MOTES POPULATION) from best fitness to worst fitness."
  (setf (motes population) (sort (motes population) #'compare-fitness)))
