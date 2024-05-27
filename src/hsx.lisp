(uiop:define-package #:hsx/hsx
  (:use #:cl)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:defhsx
           #:defcomp
           #:hsx))
(in-package #:hsx/hsx)


;;;; hsx definitions

(defmacro defhsx (name element-type)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(defmacro ,name (&body body)
       (multiple-value-bind (props children)
           (parse-body body)
         `(create-element ,',element-type (list ,@props) ,@children)))))

(defun parse-body (body)
  (if (keywordp (first body))
      (loop :for thing :on body :by #'cddr
            :for (k v) := thing
            :when (and (keywordp k) v)
            :append (list k v) :into props
            :when (not (keywordp k))
            :return (values props thing)
            :finally (return (values props nil)))
      (values nil body)))

(defmacro defcomp (name props &body body)
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defhsx ,name (fdefinition ',%name)))))


;;;; hsx macro to find hsx symbols

(defmacro hsx (&body form)
  (when (not (= (length form) 1))
    (error "The body of the hsx macro must be a single form."))
  (find-builtin-symbols (car form)))

(defun find-builtin-symbols (node)
  (if (atom node)
      (or (find-symbol (string node) :hsx/builtin)
          node)
      (cons (find-builtin-symbols (car node))
            (mapcar (lambda (n)
                      (if (listp n)
                          (find-builtin-symbols n)
                          n))
                    (cdr node)))))
