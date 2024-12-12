(defpackage #:hsx/dsl
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:hsx
           #:deftag
           #:defcomp))
(in-package #:hsx/dsl)

;;;; hsx macro

(defmacro hsx (form)
  "Detect HSX elements and automatically import them."
  (detect-elements form))

(defun get-builtin-symbol (sym)
  (multiple-value-bind (builtin-sym kind)
      (find-symbol (string sym) :hsx/builtin)
    (and (eq kind :external) builtin-sym)))

(defun start-with-tilde-p (sym)
  (string= "~" (subseq (string sym) 0 1)))

(defun get-component-symbol (sym)
  (and (start-with-tilde-p sym) sym))

(defun detect-elements (form)
  (check-type form cons)
  (let* ((head (first form))
         (tail (rest form))
         (well-formed-p (listp tail))
         (detected-sym (and (symbolp head)
                            (not (keywordp head))
                            (or (get-builtin-symbol head)
                                (get-component-symbol head)))))
    (if (and well-formed-p detected-sym)
        (cons detected-sym
              (mapcar (lambda (sub-form)
                        (if (consp sub-form)
                            (detect-elements sub-form)
                            sub-form))
                      tail))
        form)))

;;;; defhsx macro

(defmacro defhsx (name element-type)
  `(defmacro ,name (&body body)
     `(%create-element ,',element-type ,@body)))

(defun %create-element (type &rest body)
  (multiple-value-bind (props children)
      (parse-body body)
    (create-element type props children)))

(defun parse-body (body)
  (cond ((and (listp (first body))
              (keywordp (first (first body))))
         (values (first body) (rest body)))
        ((keywordp (first body))
         (loop :for thing :on body :by #'cddr
               :for (k v) := thing
               :when (and (keywordp k) v)
               :append (list k v) :into props
               :when (not (keywordp k))
               :return (values props thing)
               :finally (return (values props nil))))
        (t (values nil body))))

(defmacro deftag (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defhsx ,name ,(make-keyword name))))

(defmacro defcomp (~name props &body body)
  "Define a function component for HSX.
The component name must start with a tilde (~).
Properties must be declared using &key, &rest, or both.
The body must return an HSX element."
  (unless (start-with-tilde-p ~name)
    (error "The component name must start with a tilde (~~)."))
  (unless (or (null props)
              (member '&key props)
              (member '&rest props))
    (error "Component properties must be declared with either &key, &rest, or both."))
  (let ((%name (symbolicate '% ~name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defhsx ,~name (fdefinition ',%name)))))
