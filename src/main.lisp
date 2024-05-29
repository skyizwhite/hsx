(defpackage :hsx
  (:nicknames #:hsx/main)
  (:use #:cl
        #:hsx/element
        #:hsx/defhsx
        #:hsx/hsx)
  (:import-from #:hsx/builtin)
  (:export #:hsx
           #:deftag
           #:defcomp
           #:render))
(in-package :hsx)
