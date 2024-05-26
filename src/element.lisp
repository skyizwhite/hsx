(defpackage #:hsx/element
  (:use #:cl)
  (:export #:element-type
           #:element-props
           #:element-children
           #:create-element
           #:expand))
(in-package #:hsx/element)

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

(defun create-element (type props &rest children)
  (let ((elm (make-instance 'element
                            :type type
                            :props props
                            :children (flatten children))))
    (prog1 elm
      ;dry-run to validate props
      (expand elm))))

(defmethod expand ((elm element))
  (with-accessors ((type element-type)
                   (props element-props)
                   (children element-children)) elm
    (if (functionp type)
        (apply type (append props
                            (and children
                                 (list :children children))))
        elm)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))
