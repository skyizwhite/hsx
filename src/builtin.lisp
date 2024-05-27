(uiop:define-package #:hsx/builtin
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:hsx/hsx
                #:defhsx))
(in-package #:hsx/builtin)

(defmacro define-and-export-builtin-elements (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(defhsx ,name ,(string-downcase name))
                       `(export ',name)))
               names)))

(define-and-export-builtin-elements
    ; tag-elements
    a abbr address area article aside audio b base bdi bdo blockquote
  body br button canvas caption cite code col colgroup data datalist
  dd del details dfn dialog div dl dt em embed fieldset figcaption
  figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe
  img input ins kbd label legend li link main |map| mark meta meter nav
  noscript object ol optgroup option output p param picture pre progress
  q rp rt ruby s samp script section select small source span strong
  style sub summary sup svg table tbody td template textarea tfoot th
  thead |time| title tr track u ul var video wbr
  ; html-tag-element
  html
  ; fragment-element
  <>)
