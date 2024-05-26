(uiop:define-package #:hsx/hsx
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate
                #:make-keyword)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:defcomp
           #:hsx))
(in-package #:hsx/hsx)


;;;; hsx definitions

(defparameter *builtin-elements* (make-hash-table))

(defmacro define-builtin-element (name)
  `(defmacro ,name (&body body)
     (multiple-value-bind (props children)
         (parse-body body)
       `(create-element ,',(string-downcase name)
                        (list ,@props)
                        ,@children))))

(defmacro define-and-export-builtin-elements (&body names)
  `(progn
     ,@(mapcan (lambda (name)
                 (list `(define-builtin-element ,name)
                       `(setf (gethash (make-keyword ',name) *builtin-elements*) t)
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

(defmacro defcomp (name props &body body)
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defmacro ,name (&body body)
         (multiple-value-bind (props children)
             (parse-body body)
           `(create-element #',',%name
                            (list ,@props)
                            ,@children))))))

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


;;;; hsx macro to find hsx symbols

(defmacro hsx (&body body)
  `(progn
     ,@(modify-first-leaves body
                            #'builtin-element-p
                            (lambda (node)
                              (find-symbol (string node) :hsx/hsx)))))

(defun modify-first-leaves (tree test result)
  (if tree
      (cons (let ((first-node (first tree)))
              (cond
                ((listp first-node)
                 (modify-first-leaves first-node test result))
                ((funcall test first-node)
                 (funcall result first-node))
                (t first-node)))
            (mapcar (lambda (node)
                      (if (listp node)
                          (modify-first-leaves node test result)
                          node))
                    (rest tree)))))

(defun builtin-element-p (node)
  (and (symbolp node)
       (gethash (make-keyword node) *builtin-elements*)))
