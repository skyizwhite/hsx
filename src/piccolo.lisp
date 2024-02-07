(in-package :piccolo)

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
                 :children (escape-children children)))

(defun make-builtin-element-with-prefix (&key tag attrs children prefix)
  (make-instance 'builtin-element-with-prefix
                 :tag tag
                 :attrs attrs
                 :prefix prefix
                 :children (escape-children children)))

(defun make-user-element (&key tag attrs children expander)
  (make-instance 'user-element
                 :tag tag
                 :attrs attrs
                 :expander expander
                 :children (escape-children children)))

(defmethod user-element-expand-to ((element user-element))
  (funcall (user-element-expander element)
           (element-tag element)
           (element-attrs element)
           (element-children element)))

(defun make-fragment (&key children)
  (make-instance 'fragment
                 :tag 'fragment
                 :attrs (make-attrs :alist nil)
                 :children (escape-children children)))

(defstruct (attrs (:constructor %make-attrs))
  alist)

(defvar *escape-html* :utf8
  "Specify the escape option when generate html, can be :UTF8, :ASCII, :ATTR or NIL.
If :UTF8, escape only #\<, #\> and #\& in body, and \" in attribute keys. #\' will
in attribute keys will not be escaped since piccolo will always use double quote for
attribute keys.
If :ASCII, besides what escaped in :UTF8, also escape all non-ascii characters.
If :ATTR, only #\" in attribute values will be escaped.
If NIL, nothing is escaped and programmer is responsible to escape elements properly.
When given :ASCII and :ATTR, it's possible to insert html text as a children, e.g.
(div :id \"container\" \"Some <b>text</b>\")")

(defun make-attrs (&key alist)
  (if *escape-html*
      (%make-attrs :alist (escape-attrs-alist alist))
      (%make-attrs :alist alist)))

(defmethod (setf attr) (value (attrs attrs) key)
  (setf (aget (attrs-alist attrs) key) value))

(defmethod delete-attr ((attrs attrs) key)
  (delete-from-alistf (attrs-alist attrs) key))

(defmethod attr ((attrs attrs) key)
  (aget (attrs-alist attrs) key))

(defmethod (setf attr) (value (element element) key)
  (setf (attr (element-attrs element) key) value))

(defmethod delete-attr ((element element) key)
  (delete-attr (element-attrs element) key))

(defmethod attr ((element element) key)
  (attr (element-attrs element) key))

(defvar *builtin-elements* (make-hash-table))

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
  (let ((%element-name (alexandria:symbolicate '% element-name)))
    `(progn
       (defun ,%element-name (&rest attrs-and-children)
         (multiple-value-bind (attrs children)
             (split-attrs-and-children attrs-and-children)
           (make-builtin-element :tag (string-downcase (mkstr ',element-name))
                                 :attrs attrs
                                 :children children)))
       (defmacro ,element-name (&body attrs-and-children)
         `(,',%element-name ,@attrs-and-children)))))

(defmacro define-and-export-builtin-elements (&rest element-names)
  `(progn
     ,@(mapcan (lambda (e)
                 (list `(define-builtin-element ,e)
                       `(setf (gethash (make-keyword ',e) *builtin-elements*) t)
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
      (format stream " ~{~a=~s~^ ~}" (alist-plist (attrs-alist attrs)))
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

(defmacro! define-element (name (&rest args) &body body)
  (let ((%name (alexandria:symbolicate '% name)))
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
                                          (list arg `(attr attrs (make-keyword ',arg))))
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

(defun html-element-p (x)
  (and (symbolp x) (not (keywordp x)) (gethash (make-keyword x) *builtin-elements*)))

(defmacro h (&body body)
  `(progn
     ,@(modify-first-leaves
        body
        (or (html-element-p x) (string= x '<>))
        (find-symbol (string-upcase x) :piccolo))))

(defmethod element-string ((element element))
  (with-output-to-string (s)
    (write element :stream s)))

(defmethod elem-str ((element element))
  (with-output-to-string (s)
    (write element :stream s :pretty nil)))
