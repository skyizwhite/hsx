(defpackage #:hsx/element
  (:use #:cl)
  (:export #:element-type
           #:element-props
           #:element-children
           #:create-element
           #:expand))
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

(defclass builtin-element (element) ())

(defclass tag-element (builtin-element) ())

(defclass html-tag-element (tag-element) ())

(defclass fragment-element (builtin-element) ())

(defclass component-element (element) ())


;;;; constructor

(defun create-element (type props &rest children)
  (let ((elm (make-instance (cond ((functionp type) 'component-element)
                                  ((string= type "<>") 'fragment-element)
                                  ((string= type "html") 'html-tag-element)
                                  (t 'tag-element))
                            :type type
                            :props props
                            :children (flatten children))))
    (create-element-hook elm)
    elm))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

(defmethod create-element-hook ((elm element)))

(defmethod create-element-hook ((elm fragment-element))
  (when (element-props elm)
    (error "Cannot pass props to fragment.")))

(defmethod create-element-hook ((elm component-element))
  ;dry-run to validate props
  (expand elm))


;;;; methods

(defmethod expand ((elm component-element))
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) elm
    (apply type (merge-children-into-props props children))))

(defun merge-children-into-props (props children)
  (append props
          (and children
               (list :children children))))

(defmethod print-object ((elm tag-element) stream)
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) elm
    (if children
        (format stream (if (rest children)
                           "~@<<~a~a>~2I~:@_~<~@{~a~^~:@_~}~:>~0I~:@_</~a>~:>"
                           "~@<<~a~a>~2I~:_~<~a~^~:@_~:>~0I~_</~a>~:>")
                type
                (props->string props)
                children
                type)
        (format stream "<~a~a></~a>"
                type
                (props->string props)
                type))))

(defun props->string (props)
  (with-output-to-string (stream)
    (loop
      :for (key value) :on props :by #'cddr
      :do (format stream (if (typep value 'boolean)
                             "~@[ ~a~]"
                             " ~a=~s")
                  (string-downcase key)
                  value))))

(defmethod print-object ((elm html-tag-element) stream)
  (format stream "<!DOCTYPE html>~%")
  (call-next-method))

(defmethod print-object ((elm fragment-element) stream)
  (with-accessors ((children element-children)) elm
    (if children
        (format stream (if (rest children)
                           "~<~@{~a~^~:@_~}~:>"
                           "~<~a~:>")
                children))))

(defmethod print-object ((elm component-element) stream)
  (print-object (expand elm) stream))
