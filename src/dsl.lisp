(defpackage #:hsx/dsl
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:import-from #:hsx/element
                #:create-element)
  (:export #:hsx
           #:deftag
           #:register-web-components
           #:clear-web-components
           #:defcomp))
(in-package #:hsx/dsl)

;;; hsx macro

(defmacro hsx (form)
  "Automatically detect html tags, registered Web Components, and user-defined HSX components.
All other expressions are evaluated as regular Lisp forms.
To create HSX elements within a Lisp form, use the `hsx` macro again."
  (detect-elements form))

(defun external-symbol (sym package)
  (multiple-value-bind (s kind)
      (find-symbol (string sym) package)
    (and (eq kind :external) s)))

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
                                      (or (external-symbol head :hsx/web-components)
                                          (external-symbol head :hsx/builtin)
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

(defmacro register-web-components (&rest names)
  (let ((pkg (find-package :hsx/web-components)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(mapcan (lambda (name)
                   (let ((sym (intern (string name) pkg)))
                     (list `(deftag ,sym)
                           `(export ',sym ',pkg))))
                 names))))

(defun clear-web-components ()
  (let ((pkg :hsx/web-components))
    (do-external-symbols (sym pkg)
      (unintern sym pkg))))

(defun key-var-keyword (spec)
  "Return the keyword name of a &key lambda-list variable SPEC, which may be a
plain symbol, `(var default)`, or `((:keyword var) default)`."
  (let ((name (if (consp spec) (first spec) spec)))
    (if (consp name)
        (first name)
        (make-keyword name))))

(defun rest-prop-symbol-p (sym)
  "True if SYM is the special `rest` prop (a plain symbol named REST), as opposed
to the &rest lambda-list keyword."
  (and (symbolp sym)
       (not (member sym lambda-list-keywords))
       (string= (symbol-name sym) "REST")))

(defun parse-prop-keys (props)
  "Inspect a component props lambda list and return (values declared-keywords
uses-rest-p). DECLARED-KEYWORDS holds the keyword name of every &key variable
except the special `rest` prop; USES-REST-P is true when `rest` is declared."
  (let ((key-pos (position '&key props))
        (uses-rest nil)
        (kws '()))
    (when key-pos
      (loop :for x :in (nthcdr (1+ key-pos) props)
            :until (member x lambda-list-keywords)
            :do (if (rest-prop-symbol-p x)
                    (setf uses-rest t)
                    (push (key-var-keyword x) kws))))
    (values (nreverse kws) uses-rest)))

(defun keep-props (plist keys)
  "Return a fresh plist with only the entries of PLIST whose key is in KEYS."
  (loop :for (k v) :on plist :by #'cddr
        :when (member k keys)
        :append (list k v)))

(defun remove-props (plist keys)
  "Return a fresh plist with only the entries of PLIST whose key is not in KEYS."
  (loop :for (k v) :on plist :by #'cddr
        :unless (member k keys)
        :append (list k v)))

(defmacro defcomp (~name props &body body)
  "Define an HSX component:
- name: must begin with a tilde (~)
- props: must be declared using &key, &rest, or both
         the `children` key receives the component’s child elements
         the special `rest` key, declared in the &key section, receives every
         prop not explicitly declared, gathered into a plist (like React's
         `...rest`)
- body: must return a valid HSX element"
  (unless (start-with-tilde-p ~name)
    (error "The component name must start with a tilde (~~)."))
  (unless (or (null props)
              (member '&key props)
              (member '&rest props))
    (error "Component properties must be declared using &key, &rest, or both."))
  (let ((%name (symbolicate '% ~name)))
    (multiple-value-bind (declared-keys uses-rest) (parse-prop-keys props)
      (if uses-rest
          (let ((%impl (symbolicate '%% ~name)))
            `(eval-when (:compile-toplevel :load-toplevel :execute)
               (defun ,%impl ,props ,@body)
               (defun ,%name (&rest all-props)
                 (apply #',%impl
                        (nconc (keep-props all-props ',declared-keys)
                               (list :rest (remove-props all-props
                                                          ',declared-keys)))))
               (defhsx ,~name (fdefinition ',%name))))
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (defun ,%name ,props ,@body)
             (defhsx ,~name (fdefinition ',%name)))))))
