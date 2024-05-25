(defpackage #:hsx/element
  (:use #:cl)
  (:export #:element-kind
           #:element-props
           #:create-element
           #:expand))
(in-package #:hsx/element)

(defclass element ()
  ((kind
    :reader element-kind
    :initarg :kind)
   (props
    :reader element-props
    :initarg :props)))

(defun create-element (kind props &rest children)
  (make-instance 'element
                 :kind kind
                 :props (append props
                                (and children
                                     (list :children (flatten children))))))

(defmethod expand ((elm element))
  (with-accessors ((kind element-kind)
                   (props element-props)) elm
    (if (functionp kind)
        (apply kind props)
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
