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

;;; hsx macro

(defmacro hsx (form)
  "Automatically detect built-in tags and user-defined components.
All other expressions are evaluated as regular Lisp forms.
To create HSX elements within a Lisp form, use the `hsx` macro again."
  (detect-elements form))

(defun detect-builtin-element (sym)
  (multiple-value-bind (builtin-sym kind)
      (find-symbol (string sym) :hsx/builtin)
    (and (eq kind :external) builtin-sym)))

(defun start-with-tilde-p (sym)
  (string= "~" (subseq (string sym) 0 1)))

(defun detect-component (sym)
  (and (start-with-tilde-p sym) sym))

(defun detect-elements (form)
  (or (and (consp form)
           (listp (rest form))
           (let* ((head (first form))
                  (tail (rest form))
                  (detected-head (and (symbolp head)
                                      (not (keywordp head))
                                      (or (detect-builtin-element head)
                                          (detect-component head)))))
             (and detected-head
                  (cons detected-head (mapcar #'detect-elements tail)))))
      form))

;;; defhsx macro

(defmacro defhsx (name element-type)
  ; Use a macro instead of a function to allow semantic indentation, similar to HTML.
  `(defmacro ,name (&body body)
     `(%create-element ,',element-type ,@body)))

(defun %create-element (type &rest body)
  (multiple-value-bind (props children)
      (parse-body body)
    (create-element type props children)))

(defun parse-body (body)
  (cond
    ; body has props as a normal plist
    ((plist-p (first body))
     (values (first body) (rest body)))
    ; body has props as an inline plist
    ((keywordp (first body))
     (loop :for thing :on body :by #'cddr
           :for (k v) := thing
           :when (and (keywordp k) v)
           :append (list k v) :into props
           :when (not (keywordp k))
           :return (values props thing)
           :finally (return (values props nil))))
    ; body has no props
    (t (values nil body))))

(defun plist-p (obj)
  (and (listp obj)
       (evenp (length obj))
       (loop :for (key _) :on obj :by #'cddr
             :always (symbolp key))))

(defmacro deftag (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defhsx ,name ,(make-keyword name))))

(defmacro defcomp (~name props &body body)
  "Define an HSX component:
- name: must begin with a tilde (~)
- props: must be declared using &key, &rest, or both
         the `children` key receives the component’s child elements
- body: must return a valid HSX element"
  (unless (start-with-tilde-p ~name)
    (error "The component name must start with a tilde (~~)."))
  (unless (or (null props)
              (member '&key props)
              (member '&rest props))
    (error "Component properties must be declared using &key, &rest, or both."))
  (let ((%name (symbolicate '% ~name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,%name ,props ,@body)
       (defhsx ,~name (fdefinition ',%name)))))
