(uiop:define-package :piccolo
  (:nicknames #:piccolo/main)
  (:use #:cl)
  (:use-reexport #:piccolo/elements)
  (:use-reexport #:piccolo/generator)
  (:use-reexport #:piccolo/util))
(in-package :piccolo)
