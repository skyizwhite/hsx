(defpackage #:hsx/element
  (:use #:cl)
  (:export #:create-element
           #:element-type
           #:element-props
           #:element-children
           #:expand-component
           #:render))
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

(defclass fragment (element) ())

(defclass component (element) ())


;;;; factory

(defun create-element (type props &rest children)
  (let ((element (make-instance (cond ((functionp type) 'component)
                                      ((eq type :<>) 'fragment)
                                      ((eq type :html) 'html-tag)
                                      ((keywordp type) 'tag)
                                      (t (error "element-type must be either a keyword or a function.")))
                                :type type
                                :props props
                                :children (flatten children))))
    (create-element-hook element)
    element))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defmethod create-element-hook ((element element)))

(defmethod create-element-hook ((element fragment))
  (when (element-props element)
    (error "Cannot pass props to fragment.")))

(defmethod create-element-hook ((element component))
  ;dry-run to validate props
  (expand-component element))


;;;; methods

(defmethod print-object ((element tag) stream)
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) element
    (let ((type-str (string-downcase type)))
      (if children
          (format stream
                  (if (rest children)
                      "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                      "~@<<~a~a>~2I~:_~<~a~^~:@_~:>~0I~_</~a>~:>")
                  type-str
                  (props->string props)
                  children
                  type-str)
          (format stream
                  "<~a~a></~a>"
                  type-str
                  (props->string props)
                  type-str)))))

(defun props->string (props)
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
                        value))))))

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

(defmethod render ((element element) &key minify)
  (with-output-to-string (stream)
    (write element :stream stream :pretty (not minify))))
