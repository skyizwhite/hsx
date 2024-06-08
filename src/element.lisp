(defpackage #:hsx/element
  (:use #:cl)
  (:import-from #:hsx/escaper
                #:escape-html-attribute
                #:escape-html-text-content)
  (:import-from #:hsx/group
                #:self-closing-tag-p
                #:non-escaping-tag-p)
  (:export #:element
           #:tag
           #:html-tag
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

(defclass fragment (tag) ())

(defclass component (element) ())

;;;; factory

(defun create-element (type props children)
  (make-instance 
   (cond ((functionp type) 'component)
         ((eq type :<>) 'fragment)
         ((eq type :html) 'html-tag)
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

(defmethod render-to-string ((element element) &key pretty)
  (with-output-to-string (stream)
    (write element :stream stream :pretty pretty)))

(defmethod print-object ((element tag) stream)
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) element
    (let ((type-str (string-downcase type))
          (props-str (render-props props)))
      (if children
          (format stream
                  (if (or (rest children)
                          (typep (first children) 'element))
                      "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                      "~@<<~a~a>~2I~:_~<~a~^~:@_~:>~0I~_</~a>~:>")
                  type-str
                  props-str
                  (escape-children type children)
                  type-str)
          (format stream
                  (if (self-closing-tag-p type)
                      "<~a~a>"
                      "<~a~a></~a>")
                  type-str
                  props-str
                  type-str)))))

(defun render-props (props)
  (with-output-to-string (stream)
    (loop
      :for (key value) :on props :by #'cddr
      :do (let ((key-str (string-downcase key)))
            (if (typep value 'boolean)
                (format stream
                        "~@[ ~a~]"
                        (and value key-str))
                (format stream
                        " ~a=\"~a\""
                        key-str
                        (escape-html-attribute value)))))))

(defun escape-children (type children)
  (mapcar (lambda (child)
            (if (and (not (non-escaping-tag-p type))
                     (stringp child))
                (escape-html-text-content child)
                child))
          children))

(defmethod print-object ((element html-tag) stream)
  (format stream "<!DOCTYPE html>~%")
  (call-next-method))

(defmethod print-object ((element fragment) stream)
  (with-accessors ((children element-children)) element
    (if children
        (format stream
                (if (rest children)
                    "~<~@{~a~^~:@_~}~:>"
                    "~<~a~:>")
                children))))

(defmethod print-object ((element component) stream)
  (print-object (expand-component element) stream))

(defmethod expand-component ((element component))
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) element
    (apply type (merge-children-into-props props children))))

(defun merge-children-into-props (props children)
  (append props
          (and children
               (list :children children))))
