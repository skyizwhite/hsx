(uiop:define-package #:hsx/hsx
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:defcomp))
(in-package #:hsx/hsx)

(defun parse-body (body)
  (if (keywordp (first body))
      (loop :for thing :on body :by #'cddr
            :for (k v) := thing
            :when (and (keywordp k) v)
            :append (list k v) :into props
            :when (not (keywordp k))
            :return (values props thing)
            :finally (return (values props nil)))
      (values nil body)))

(defmacro define-builtin-element (name)
  `(defmacro ,name (&body body)
     (multiple-value-bind (props children)
         (parse-body body)
       `(create-element ,',(string-downcase name)
                        ',props
                        ,@children))))

(defmacro define-and-export-builtin-elements (&rest names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(define-builtin-element ,name)
                       `(export ',name)))
               names)))

(define-and-export-builtin-elements
    a abbr address area article aside audio b base bdi bdo blockquote
  body br button canvas caption cite code col colgroup data datalist
  dd del details dfn dialog div dl dt em embed fieldset figcaption
  figure footer form h1 h2 h3 h4 h5 h6 head header html hr i iframe
  img input ins kbd label legend li link main |map| mark meta meter nav
  noscript object ol optgroup option output p param picture pre progress
  q rp rt ruby s samp script section select small source span strong
  style sub summary sup svg table tbody td template textarea tfoot th
  thead |time| title tr track u ul var video wbr)

(defmacro defcomp (name props &body body)
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defmacro ,name (&body body)
         (multiple-value-bind (props children)
             (parse-body body)
           `(create-element #',',%name
                            ',props
                            ,@children))))))
