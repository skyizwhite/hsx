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
  "Detect built-in HSX elements and automatically import them."
  (find-builtin-symbols form))

(defun get-builtin-symbol (sym)
  (multiple-value-bind (builtin-sym kind)
      (find-symbol (string sym) :hsx/builtin)
    (and (eq kind :external) builtin-sym)))

(defun find-builtin-symbols (form)
  (check-type form cons)
  (let* ((head (first form))
         (tail (rest form))
         (well-formed-p (listp tail))
         (builtin-sym (and (symbolp head)
                           (not (keywordp head))
                           (get-builtin-symbol head))))
    (if (and well-formed-p builtin-sym)
        (cons builtin-sym
              (mapcar (lambda (sub-form)
                        (if (consp sub-form)
                            (find-builtin-symbols sub-form)
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

(defmacro defcomp (name props &body body)
  "Define a function component for use in HSX.
The props must be declared with either &key or &rest (or both).
The body must return an HSX element."
  (unless (or (null props)
              (member '&key props)
              (member '&rest props))
    (error "Component properties must be declared with either &key, &rest, or both."))
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defhsx ,name (fdefinition ',%name)))))
