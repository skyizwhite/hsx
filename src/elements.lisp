(uiop:define-package #:piccolo/elements
  (:use #:cl)
  (:local-nicknames (#:asu #:assoc-utils))
  (:local-nicknames (#:alx #:alexandria))
  (:local-nicknames (#:esc #:piccolo/escape))
  (:export #:html
           #:<>
           #:define-element
           #:tag
           #:children
           #:attrs
           #:attrs-alist
           #:make-attrs
           #:copy-attrs
           #:attr
           #:delete-attr
           #:element
           #:builtin-element
           #:builtin-element-with-prefix
           #:user-element
           #:fragment
           #:element-tag
           #:element-attrs
           #:element-prefix
           #:element-children
           #:user-element-expand-to
           #:h))
(in-package #:piccolo/elements)

;;; classes

(defclass element ()
  ((tag      :initarg  :tag
             :accessor element-tag)
   (attrs    :initarg  :attrs
             :accessor element-attrs)
   (children :initarg  :children
             :accessor element-children)))

(defclass builtin-element (element) ())

(defclass builtin-element-with-prefix (builtin-element)
  ((prefix :initarg  :prefix
           :accessor element-prefix)))

(defclass user-element (element)
  ((expand-to :initarg :expander
              :accessor user-element-expander)))

(defclass fragment (element) ())

;;; constructors

(defun make-builtin-element (&key tag attrs children)
  (make-instance 'builtin-element
                 :tag tag
                 :attrs attrs
                 :children (esc:escape-children children)))

(defun make-builtin-element-with-prefix (&key tag attrs children prefix)
  (make-instance 'builtin-element-with-prefix
                 :tag tag
                 :attrs attrs
                 :prefix prefix
                 :children (esc:escape-children children)))

(defun make-user-element (&key tag attrs children expander)
  (make-instance 'user-element
                 :tag tag
                 :attrs attrs
                 :expander expander
                 :children (esc:escape-children children)))

(defmethod user-element-expand-to ((element user-element))
  (funcall (user-element-expander element)
           (element-tag element)
           (element-attrs element)
           (element-children element)))

(defun make-fragment (&key children)
  (make-instance 'fragment
                 :tag "fragment"
                 :attrs (make-attrs :alist nil)
                 :children (esc:escape-children children)))

;;; attributes

(defstruct (attrs (:constructor %make-attrs))
  alist)

(defun make-attrs (&key alist)
  (if esc:*escape-html*
      (%make-attrs :alist (esc:escape-attrs-alist alist))
      (%make-attrs :alist alist)))

(defmethod (setf attr) (value (attrs attrs) key)
  (setf (asu:aget (attrs-alist attrs) key) value))

(defmethod delete-attr ((attrs attrs) key)
  (asu:delete-from-alistf (attrs-alist attrs) key))

(defmethod attr ((attrs attrs) key)
  (asu:aget (attrs-alist attrs) key))

(defmethod (setf attr) (value (element element) key)
  (setf (attr (element-attrs element) key) value))

(defmethod delete-attr ((element element) key)
  (delete-attr (element-attrs element) key))

(defmethod attr ((element element) key)
  (attr (element-attrs element) key))

;;; elements

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defun split-attrs-and-children (attrs-and-children)
  (cond
    ((attrs-p (first attrs-and-children))
     (values (first attrs-and-children) (flatten (rest attrs-and-children))))
    ((asu:alistp (first attrs-and-children))
     (values (make-attrs :alist (first attrs-and-children))
             (flatten (rest attrs-and-children))))
    ((listp (first attrs-and-children)) ;plist
     (values (make-attrs :alist (alx:plist-alist (first attrs-and-children)))
             (flatten (rest attrs-and-children))))
    ((hash-table-p (first attrs-and-children))
     (values (make-attrs :alist (asu:hash-alist (first attrs-and-children)))
             (flatten (rest attrs-and-children))))
    ((keywordp (first attrs-and-children)) ;inline-plist
     (loop :for thing :on attrs-and-children :by #'cddr
           :for (k v) := thing
           :when (and (keywordp k) v)
           :collect (cons k v) :into attrs
           :when (not (keywordp k))
           :return (values (make-attrs :alist attrs) (flatten thing))
           :finally (return (values (make-attrs :alist attrs) nil))))
    (t
     (values (make-attrs :alist nil) (flatten attrs-and-children)))))

(defparameter *builtin-elements* (make-hash-table))
(setf (gethash :html *builtin-elements*) t)

(defun %html (&rest attrs-and-children)
  (multiple-value-bind (attrs children)
      (split-attrs-and-children attrs-and-children)
    (make-builtin-element-with-prefix :tag "html"
                                      :attrs attrs
                                      :children children
                                      :prefix "<!DOCTYPE html>")))

(defmacro html (&body attrs-and-children)
  `(%html ,@attrs-and-children))

(defmacro define-builtin-element (element-name)
  (let ((%element-name (alx:symbolicate '% element-name)))
    `(progn
       (defun ,%element-name (&rest attrs-and-children)
         (multiple-value-bind (attrs children)
             (split-attrs-and-children attrs-and-children)
           (make-builtin-element :tag (string-downcase ',element-name)
                                 :attrs attrs
                                 :children children)))
       (defmacro ,element-name (&body attrs-and-children)
         `(,',%element-name ,@attrs-and-children)))))

(defmacro define-and-export-builtin-elements (&rest element-names)
  `(progn
     ,@(mapcan (lambda (e)
                 (list `(define-builtin-element ,e)
                       `(setf (gethash (alx:make-keyword ',e) *builtin-elements*) t)
                       `(export ',e)))
               element-names)))

(define-and-export-builtin-elements
    a abbr address area article aside audio b base bdi bdo blockquote
  body br button canvas caption cite code col colgroup data datalist
  dd del details dfn dialog div dl dt em embed fieldset figcaption
  figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe
  img input ins kbd label legend li link main |map| mark meta meter nav
  noscript object ol optgroup option output p param picture pre progress
  q rp rt ruby s samp script section select small source span strong
  style sub summary sup svg table tbody td template textarea tfoot th
  thead |time| title tr track u ul var video wbr)

(defmacro define-element (name (&rest args) &body body)
  (let ((%name (alx:symbolicate '% name))
        (attrs (gensym "attrs"))
        (children (gensym "children"))
        (exp-children (gensym "exp-children")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name (&rest attrs-and-children)
         (multiple-value-bind (,attrs ,children)
             (split-attrs-and-children attrs-and-children)
           (make-user-element 
            :tag (string-downcase ',name)
            :attrs ,attrs
            :children ,children
            :expander (lambda (tag attrs ,exp-children)
                        (declare (ignorable tag attrs))
                        (let ((children (and ,exp-children
                                             (make-fragment :children ,exp-children))))
                          (declare (ignorable children))
                          (let ,(mapcar (lambda (arg)
                                          (list arg `(attr attrs (alx:make-keyword ',arg))))
                                        args)
                            (progn ,@body)))))))
       (defmacro ,name (&body attrs-and-children)
         `(,',%name ,@attrs-and-children)))))

(defun %<> (&rest attrs-and-children)
  (multiple-value-bind (attrs children)
      (split-attrs-and-children attrs-and-children)
    (declare (ignore attrs))
    (make-fragment :children children)))

(defmacro <> (&body children)
  `(%<> ,@children))

;;; h macro

(defun html-element-p (node)
  (and (symbolp node)
       (not (keywordp node))
       (gethash (alx:make-keyword node) *builtin-elements*)))

(defun fragment-p (node)
  (string= node '<>))

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

(defmacro h (&body body)
  `(progn
     ,@(modify-first-leaves
        body
        (lambda (node)
          (or (html-element-p node) (fragment-p node)))
        (lambda (node)
          (find-symbol (string node) :piccolo)))))
