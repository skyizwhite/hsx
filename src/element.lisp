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

(defmethod render-to-string ((element element) &key pretty)
  (with-output-to-string (stream)
    (write element :stream stream :pretty pretty)))

(defmethod print-object ((element tag) stream)
  (with-slots (type props children) element
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
                  (render-children element)
                  type-str)
          (format stream "<~a~a></~a>" type-str props-str type-str)))))

(defmethod print-object ((element self-closing-tag) stream)
  (with-slots (type props) element
    (format stream "<~a~a>" (string-downcase type) (render-props props))))

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

(defmethod render-children ((element tag))
  (mapcar (lambda (child)
            (if (stringp child)
                (escape-html-text-content child)
                child))
          (element-children element)))

(defmethod render-children ((element non-escaping-tag))
  (element-children element))

(defmethod print-object ((element html-tag) stream)
  (format stream "<!DOCTYPE html>~%")
  (call-next-method))

(defmethod print-object ((element fragment) stream)
  (with-slots (children) element
    (if children
        (format stream
                (if (rest children)
                    "~<~@{~a~^~:@_~}~:>"
                    "~<~a~:>")
                children))))

(defmethod print-object ((element component) stream)
  (print-object (expand-component element) stream))

(defmethod expand-component ((element component))
  (with-slots (type props children) element
    (apply type (merge-children-into-props props children))))

(defun merge-children-into-props (props children)
  (append props
          (and children
               (list :children children))))
