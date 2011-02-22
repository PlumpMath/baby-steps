;;;; baby-steps.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Baby Steps root directory for more info.

(in-package :cl-user)

(asdf:defsystem :baby-steps
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "classes")
                             (:file "common")
                             (:file "creation")
                             (:file "crossovers")
                             (:file "mutations")
                             (:file "evaluation")
                             (:file "baby-steps")))))
