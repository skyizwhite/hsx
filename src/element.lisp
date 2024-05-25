(defpackage #:hsx/element
  (:use #:cl)
  (:export #:element-kind
           #:element-props
           #:element-children
           #:create-element
           #:expand))
(in-package #:hsx/element)

(defclass element ()
  ((kind
    :reader element-kind
    :initarg :kind)
   (props
    :reader element-props
    :initarg :props)
   (children
    :reader element-children
    :initarg :children)))

(defun create-element (kind props &rest children)
  (make-instance 'element
                 :kind kind
                 :props props
                 :children (flatten children)))

(defmethod expand ((elm element))
  (with-accessors ((kind element-kind)
                   (props element-props)
                   (children element-children)) elm
    (if (functionp kind)
        (apply kind (append props
                            (and children
                                 (list :children children))))
        elm)))

;;;; utils

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))
