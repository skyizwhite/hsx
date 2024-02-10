(uiop:define-package :piccolo
  (:nicknames #:piccolo/main)
  (:use #:cl)
  (:use-reexport #:piccolo/escape)
  (:use-reexport #:piccolo/elements)
  (:use-reexport #:piccolo/generator))
(in-package :piccolo)
