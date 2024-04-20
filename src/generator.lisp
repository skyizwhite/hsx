(uiop:define-package #:piccolo/generator
  (:use #:cl)
  (:import-from #:piccolo/groups
                #:self-closing-tag-p)
  (:import-from #:piccolo/elements
                #:attrs
                #:attrs-alist
                #:element
                #:element-tag
                #:element-attrs
                #:element-children
                #:element-prefix
                #:builtin-element-with-prefix
                #:user-element
                #:user-element-expand-to
                #:fragment)
  (:export #:*expand-user-element*
           #:element-string
           #:elem-str))
(in-package #:piccolo/generator)

;;; print-object

(defparameter *expand-user-element* t)

(defmethod print-object ((attrs attrs) stream)
  (loop 
    :for (key . value) :in (attrs-alist attrs)
    :do (format stream (if (typep value 'boolean)
                           "~@[ ~a~]"
                           " ~a=~s")
                (string-downcase key)
                value)))

(defmethod print-object ((element element) stream)
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                         "~@<<~a~a>~2I~:_~<~a~^~:@_~:>~0I~_</~a>~:>")
              (element-tag element)
              (element-attrs element)
              (element-children element)
              (element-tag element))
      (format stream (if (self-closing-tag-p (element-tag element))
                         "<~a~a>"
                         "<~a~a></~a>")
              (element-tag element)
              (element-attrs element)
              (element-tag element))))

(defmethod print-object ((element builtin-element-with-prefix) stream)
  (format stream "~a~%" (element-prefix element))
  (call-next-method))

(defmethod print-object ((element user-element) stream)
  (if *expand-user-element*
      (print-object (user-element-expand-to element) stream)
      (call-next-method)))

(defmethod print-object ((element fragment) stream)
  (if (element-children element)
      (format stream (if (rest (element-children element))
                         "~<~@{~a~^~:@_~}~:>"
                         "~<~a~:>")
              (element-children element))))

;;; helper for generate html string

(defmethod element-string ((element element))
  (with-output-to-string (s)
    (write element :stream s :pretty t)))

(defmethod elem-str ((element element))
  (with-output-to-string (s)
    (write element :stream s :pretty nil)))
