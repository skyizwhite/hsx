(uiop:define-package #:piccolo/elements
  (:use #:cl)
  (:local-nicknames (#:util #:piccolo/util))
  (:local-nicknames (#:asc #:assoc-utils))
  (:local-nicknames (#:lol #:let-over-lambda))
  (:local-nicknames (#:alx #:alexandria))
  (:export
   ;;; builtin HTML elements
   ;;; all html5 elements, e.g. div, nav, media, export in code except
   ;;; <time> and <map> conflicts with cl symbol, are defined and
   ;;; exported as |time|, |map|
   #:html

   ;;; fragment lets you group elements without a wrapper element.
   #:<>

   ;;; user defined elements
   #:define-element
   #:*expand-user-element*
   ;; for reference tag name, attributes and children elements in user
   ;; element definition
   #:tag
   #:children
   #:attrs

   ;;; attribute accessing utilility
   #:attrs-alist
   #:make-attrs
   #:copy-attrs
   #:attr
   #:delete-attr

   ;;; element slots
   #:element-tag
   #:element-attrs
   #:element-children
   #:user-element-expand-to

   ;;; the h macro for avoiding import all builtin html element functions
   #:h
   #:element-string
   #:elem-str))
(in-package #:piccolo/elements)

(defclass element ()
  ((tag :initarg :tag
        :accessor element-tag)
   (attrs :initarg :attrs
          :accessor element-attrs)
   (children :initarg :children
             :accessor element-children)))

(defclass builtin-element (element) ())

(defclass builtin-element-with-prefix (builtin-element)
  ((prefix :initarg :prefix
           :accessor element-prefix)))

(defclass user-element (element)
  ((expand-to :initarg :expander
              :accessor user-element-expander)))

(defclass fragment (element)
  ())

(defun make-builtin-element (&key tag attrs children)
  (make-instance 'builtin-element
                 :tag tag
                 :attrs attrs
                 :children (util:escape-children children)))

(defun make-builtin-element-with-prefix (&key tag attrs children prefix)
  (make-instance 'builtin-element-with-prefix
                 :tag tag
                 :attrs attrs
                 :prefix prefix
                 :children (util:escape-children children)))

(defun make-user-element (&key tag attrs children expander)
  (make-instance 'user-element
                 :tag tag
                 :attrs attrs
                 :expander expander
                 :children (util:escape-children children)))

(defmethod user-element-expand-to ((element user-element))
  (funcall (user-element-expander element)
           (element-tag element)
           (element-attrs element)
           (element-children element)))

(defun make-fragment (&key children)
  (make-instance 'fragment
                 :tag 'fragment
                 :attrs (make-attrs :alist nil)
                 :children (util:escape-children children)))

(defstruct (attrs (:constructor %make-attrs))
  alist)

(defun make-attrs (&key alist)
  (if util:*escape-html*
      (%make-attrs :alist (util:escape-attrs-alist alist))
      (%make-attrs :alist alist)))

(defmethod (setf attr) (value (attrs attrs) key)
  (setf (asc:aget (attrs-alist attrs) key) value))

(defmethod delete-attr ((attrs attrs) key)
  (asc:delete-from-alistf (attrs-alist attrs) key))

(defmethod attr ((attrs attrs) key)
  (asc:aget (attrs-alist attrs) key))

(defmethod (setf attr) (value (element element) key)
  (setf (attr (element-attrs element) key) value))

(defmethod delete-attr ((element element) key)
  (delete-attr (element-attrs element) key))

(defmethod attr ((element element) key)
  (attr (element-attrs element) key))

(defvar *builtin-elements* (make-hash-table))

(defun split-attrs-and-children (attrs-and-children)
  (cond
    ((attrs-p (first attrs-and-children))
     (values (first attrs-and-children) (lol:flatten (rest attrs-and-children))))
    ((asc:alistp (first attrs-and-children))
     (values (make-attrs :alist (first attrs-and-children))
             (lol:flatten (rest attrs-and-children))))
    ((listp (first attrs-and-children)) ;plist
     (values (make-attrs :alist (util:plist-alist (first attrs-and-children)))
             (lol:flatten (rest attrs-and-children))))
    ((hash-table-p (first attrs-and-children))
     (values (make-attrs :alist (asc:hash-alist (first attrs-and-children)))
             (lol:flatten (rest attrs-and-children))))
    ((keywordp (first attrs-and-children)) ;inline-plist
     (loop for thing on attrs-and-children by #'cddr
           for (k v) = thing
           when (and (keywordp k) v)
           collect (cons k v) into attrs
           when (not (keywordp k))
           return (values (make-attrs :alist attrs) (lol:flatten thing))
           finally (return (values (make-attrs :alist attrs) nil))))
    (t
     (values (make-attrs :alist nil) (lol:flatten attrs-and-children)))))

(defun %html (&rest attrs-and-children)
  (multiple-value-bind (attrs children)
      (split-attrs-and-children attrs-and-children)
    (make-builtin-element-with-prefix :tag "html"
                                      :attrs attrs
                                      :children children
                                      :prefix "<!DOCTYPE html>")))

(defmacro html (&body attrs-and-children)
  `(%html ,@attrs-and-children))

(setf (gethash :html *builtin-elements*) t)

(defmacro define-builtin-element (element-name)
  (let ((%element-name (alx:symbolicate '% element-name)))
    `(progn
       (defun ,%element-name (&rest attrs-and-children)
         (multiple-value-bind (attrs children)
             (split-attrs-and-children attrs-and-children)
           (make-builtin-element :tag (string-downcase (lol:mkstr ',element-name))
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

(defmethod print-object ((attrs attrs) stream)
  (if (attrs-alist attrs)
      (format stream " ~{~a=~s~^ ~}" (util:alist-plist (attrs-alist attrs)))
      (format stream "")))

(defparameter *self-closing-tags*
  '(area base br col embed hr img input keygen
    link meta param source track wbr))

(defun self-closing-p (tag)
  (member (make-symbol (string-upcase tag))
          *self-closing-tags*
          :test #'string=))

(defmethod print-object ((element element) stream)
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                         "~@<<~a~a>~2I~:_~<~a~:>~0I~:_</~a>~:>")
              (element-tag element)
              (element-attrs element)
              (element-children element)
              (element-tag element))
      (format stream (if (self-closing-p (element-tag element))
                         "<~a~a>"
                         "<~a~a></~a>")
              (element-tag element)
              (element-attrs element)
              (element-tag element))))

(defmethod print-object ((element builtin-element-with-prefix) stream)
  (format stream "~a~%" (element-prefix element))
  (call-next-method))

(lol:defmacro! define-element (name (&rest args) &body body)
  (let ((%name (alx:symbolicate '% name)))
    `(progn
       (defun ,%name (&rest attrs-and-children)
         (multiple-value-bind (,g!attrs ,g!children)
             (split-attrs-and-children attrs-and-children)
           (make-user-element 
            :tag (string-downcase ',name)
            :attrs ,g!attrs
            :children ,g!children
            :expander (lambda (tag attrs ,g!exp-children)
                        (declare (ignorable tag attrs ,g!exp-children))
                        (let ((children (and ,g!exp-children
                                             (make-fragment :children ,g!exp-children))))
                          (declare (ignorable children))
                          (let ,(mapcar (lambda (arg)
                                          (list arg `(attr attrs (alx:make-keyword ',arg))))
                                        args)
                            (progn ,@body)))))))
       (defmacro ,name (&body attrs-and-children)
         `(,',%name ,@attrs-and-children)))))

(defvar *expand-user-element* t)

(defmethod print-object ((element user-element) stream)
  (if *expand-user-element*
      (print-object (user-element-expand-to element) stream)
      (call-next-method)))

(defun %<> (&rest children)
  (make-fragment :children children))

(defmacro <> (&body children)
  `(%<> ,@children))

(defmethod print-object ((element fragment) stream)
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~<~@{~a~^~:@_~}~:>"
                         "~<~a~:>")
              (element-children element))))

(defun html-element-p (node)
  (and (symbolp node)
       (not (keywordp node))
       (gethash (alx:make-keyword node) *builtin-elements*)))

(defmacro h (&body body)
  `(progn
     ,@(util:modify-first-leaves
        body
        (lambda (node)
          (declare (ignorable node))
          (or (html-element-p node) (string= node '<>)))
        (lambda (node)
          (declare (ignorable node))
          (find-symbol (string-upcase node) :piccolo)))))

(defmethod element-string ((element element))
  (with-output-to-string (s)
    (write element :stream s)))

(defmethod elem-str ((element element))
  (with-output-to-string (s)
    (write element :stream s :pretty nil)))
