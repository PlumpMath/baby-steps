;;;; evaluation.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :baby-steps)


;;; Functions & Methods

(defun add-to-population (population mote)
  (vector-push-extend mote (motes population)))


(defun advance-generation (population &key (method :tournament) (elitism 0.1)
                           (crossovers 0.8) (mutations 0.1) (debug nil))
  (case method
    (:tournament (advance-generation-tournament population :elitism elitism
                                  :crossovers crossovers :mutations mutations
                                  :debug debug))
    (otherwise (error "Unknown ADVANCE-GENERATION method: ~S" method)))
  (setf (diversity population) (population-diversity population)))


(defun advance-generation-tournament (population &key (elitism 0.1)
                                      (crossovers 0.8) (mutations 0.1)
                                      (debug nil))
  "ELITISM + CROSSOVERS + MUTATIONS should add up to 1.0."
  (let ((motes (copy-seq (sort-motes population))))
    (clear-motes population)
    (when debug (format t "[AGT] --- elitism ---~%"))
    (loop for i from 0 below (ceiling (* elitism (size population)))
          do (when debug
               (format t "[AGT] Keeping ~S~%" (elt motes i)))
             (add-to-population population (elt motes i)))
    (when debug (format t "[AGT] --- crossovers ---~%"))
    (loop for i from 0 below (ceiling (* crossovers (size population)))
          for m1 = (random-elt motes)
          for m2 = (random-elt motes)
          for tree = (if (>= (fitness m1) (fitness m2))
                         (crossover (tree m1) (tree m2) :debug debug)
                         (crossover (tree m2) (tree m1) :debug debug))
          do (when debug
               (if (>= (fitness m1) (fitness m2))
                 (format t "[AGT] Crossed ~S~%            x ~S~%~S~%---~%"
                         m1 m2 tree)
                 (format t "[AGT] Crossed ~S~%            x ~S~%~S~%---~%"
                         m2 m1 tree)))
             (add-to-population population (create-mote-from-tree tree)))
    (when debug (format t "[AGT] --- mutations ---~%"))
    (loop for i from 0 below (ceiling (* mutations (size population)))
          for m1 = (random-elt motes)
          for m2 = (random-elt motes)
          for tree = (if (>= (fitness m1) (fitness m2))
                         (mutate (tree m1) (operators population)
                                 (terminals population) :debug debug)
                         (mutate (tree m2) (operators population)
                                 (terminals population) :debug debug))
          do (when debug
               (if (>= (fitness m1) (fitness m2))
                 (format t "[AGT] Mutated ~S~%~S~%---~%" m1 tree)
                 (format t "[AGT] Mutated ~S~%~S~%---~%" m2 tree)))
             (add-to-population population (create-mote-from-tree tree))
             (when (>= (length (motes population)) (size population))
               (loop-finish)))))


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


(defun clear-motes (population)
  (setf (fill-pointer (motes population)) 0))


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


;; XXX maybe just find max fitness in pop and use that for calcs?
(defun normalise-fitness (population)
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


;; Doesn't actually ever reach 0 since a P of 100 will have max 99 dups.
(defun population-diversity (population)
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


(defun sort-motes (population)
  "Sorts (MOTES POPULATION) from best fitness to worst fitness."
  (setf (motes population) (sort (motes population) #'compare-fitness)))
