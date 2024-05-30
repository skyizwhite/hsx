(defpackage #:hsx/defhsx
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:deftag
           #:defcomp))
(in-package #:hsx/defhsx)

(defun %create-element (type &rest body)
  (multiple-value-bind (props children)
      (parse-body body)
    (create-element type props children)))

(defmacro defhsx (name element-type)
  `(defmacro ,name (&body body)
     `(%create-element ,',element-type ,@body)))

(defun parse-body (body)
  (cond (; plist
         (and (listp (first body))
              (keywordp (first (first body))))
         (values (first body) (rest body)))
        (; inline-plist
         (keywordp (first body))
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
  (let ((%name (symbolicate '% name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props
         ,@body)
       (defhsx ,name (fdefinition ',%name)))))
