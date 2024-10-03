(defpackage #:hsx/element
  (:use #:cl)
  (:import-from #:hsx/utils
                #:escape-html-attribute
                #:escape-html-text-content
                #:minify)
  (:import-from #:hsx/group
                #:self-closing-tag-p
                #:non-escaping-tag-p)
  (:export #:element
           #:tag
           #:html-tag
           #:self-closing-tag
           #:non-escaping-tag
           #:fragment
           #:component
           #:create-element
           #:element-type
           #:element-props
           #:element-children
           #:expand-component
           #:render-to-string))
(in-package #:hsx/element)

;;;; class definitions

(defclass element ()
  ((type
    :reader element-type
    :initarg :type)
   (props
    :reader element-props
    :initarg :props)
   (children
    :reader element-children
    :initarg :children)))

(defclass tag (element) ())

(defclass html-tag (tag) ())

(defclass self-closing-tag (tag) ())

(defclass non-escaping-tag (tag) ())

(defclass fragment (tag) ())

(defclass component (element) ())

;;;; factory

(defun create-element (type props children)
  (make-instance
   (cond ((functionp type) 'component)
         ((eq type :<>) 'fragment)
         ((eq type :html) 'html-tag)
         ((self-closing-tag-p type) 'self-closing-tag)
         ((non-escaping-tag-p type) 'non-escaping-tag)
         ((keywordp type) 'tag)
         (t (error "element-type must be a keyword or a function.")))
   :type type
   :props props
   :children (flatten children)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

;;;; methods

(defgeneric render-to-string (element &key pretty)
  (:documentation "Render an HSX element to a string."))

(defmethod render-to-string ((element element) &key pretty)
  (with-output-to-string (stream)
    (write element :stream stream :pretty pretty)))

(defmethod print-object ((element tag) stream)
  (let ((type (render-type element))
        (props (render-props element))
        (children (render-children element)))
    (if children
        (format stream
                (if (or (rest children)
                        (typep (first children) 'element))
                    "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                    "~@<<~a~a>~2I~:_~<~a~^~:@_~:>~0I~_</~a>~:>")
                type
                props
                children
                type)
        (format stream "<~a~a></~a>" type props type))))

(defmethod print-object ((element self-closing-tag) stream)
  (format stream "<~a~a>" (render-type element) (render-props element)))

(defmethod print-object ((element html-tag) stream)
  (format stream "<!DOCTYPE html>~%")
  (call-next-method))

(defmethod print-object ((element fragment) stream)
  (let ((children (render-children element)))
    (if children
        (format stream
                (if (rest children)
                    "~<~@{~a~^~:@_~}~:>"
                    "~<~a~:>")
                children))))

(defmethod print-object ((element component) stream)
  (print-object (expand-component element) stream))

(defmethod render-type ((element tag))
  (string-downcase (element-type element)))

(defmethod render-props ((element tag))
  (minify
   (with-output-to-string (stream)
     (loop
       :for (key value) :on (element-props element) :by #'cddr
       :do (let ((key-str (string-downcase key)))
             (if (typep value 'boolean)
                 (format stream
                         "~@[ ~a~]"
                         (and value key-str))
                 (format stream
                         " ~a=\"~a\""
                         key-str
                         (escape-html-attribute value))))))))

(defmethod render-children ((element tag))
  (mapcar (lambda (child)
            (if (stringp child)
                (escape-html-text-content child)
                child))
          (element-children element)))

(defmethod render-children ((element non-escaping-tag))
  (element-children element))

(defmethod expand-component ((element component))
  (apply (element-type element) (element-props-with-children element)))

(defmethod element-props-with-children ((element component))
  (with-slots (props children) element
    (append props (and children (list :children children)))))
