(defpackage :hsx
  (:nicknames #:hsx/main)
  (:use #:cl
        #:hsx/element
        #:hsx/dsl
        #:hsx/utils)
  (:import-from #:hsx/builtin)
  (:import-from #:hsx/web-components)
  (:export #:hsx
           #:defcomp
           #:register-web-components
           #:clear-web-components
           #:render-to-string
           #:clsx))
(in-package :hsx)
