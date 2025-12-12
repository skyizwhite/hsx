(defpackage #:hsx/element
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten)
  (:import-from #:str
                #:collapse-whitespaces)
  (:import-from #:hsx/utils
                #:escape-html-text-content
                #:escape-html-attribute)
  (:export #:element
           #:tag
           #:html-tag
           #:self-closing-tag
           #:fragment
           #:raw-fragment
           #:component
           #:create-element
           #:element-type
           #:element-props
           #:element-children
           #:expand-component
           #:render-to-string))
(in-package #:hsx/element)

;;; tag group definitions

(deftype self-closing-tag-sym ()
  '(member
    :area :base :br :col :embed :hr :img :input
    :link :meta :param :source :track :wbr))

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

(defclass fragment (tag) ())

(defclass raw-fragment (fragment) ())

(defclass component (element) ())

;;;; factory

(defun create-element (type props children)
  (make-instance
   (cond ((functionp type) 'component)
         ((eq type :<>) 'fragment)
         ((eq type :raw!) 'raw-fragment)
         ((eq type :html) 'html-tag)
         ((typep type 'self-closing-tag-sym) 'self-closing-tag)
         ((keywordp type) 'tag)
         (t (error "element-type must be a keyword or a function.")))
   :type type
   :props props
   :children (flatten children)))

;;;; methods

(defgeneric render-to-string (element &key pretty)
  (:documentation "Render an HSX element to an HTML string."))

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
                        (typep (first children) '(and element (not fragment))))
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
  (collapse-whitespaces
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

(defmethod render-children ((element raw-fragment))
  (element-children element))

(defmethod expand-component ((element component))
  (apply (element-type element) (element-props-with-children element)))

(defmethod element-props-with-children ((element component))
  (with-slots (props children) element
    (append props (and children (list :children children)))))
