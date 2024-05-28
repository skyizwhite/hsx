(uiop:define-package :hsx
  (:nicknames #:hsx/main)
  (:use #:cl)
  (:use-reexport #:hsx/defhsx)
  (:import-from #:hsx/builtin)
  (:use-reexport #:hsx/hsx))
(in-package :hsx)
