(uiop:define-package #:hsx/builtin
  (:use #:cl)
  (:import-from #:hsx/hsx
                #:deftag))
(in-package #:hsx/builtin)

(defmacro define-builtin-tags (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(deftag ,name)
                       `(export ',name)))
               names)))

(define-builtin-tags
    a abbr address area article aside audio b base bdi bdo blockquote
  body br button canvas caption cite code col colgroup data datalist
  dd del details dfn dialog div dl dt em embed fieldset figcaption
  figure footer form h1 h2 h3 h4 h5 h6 head header html hr i iframe
  img input ins kbd label legend li link main |map| mark meta meter nav
  noscript object ol optgroup option output p param picture pre progress
  q rp rt ruby s samp script section select small source span strong
  style sub summary sup svg table tbody td template textarea tfoot th
  thead |time| title tr track u ul var video wbr <>)
