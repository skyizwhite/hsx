(defpackage #:hsx/hsx
  (:use #:cl)
  (:export #:hsx))
(in-package #:hsx/hsx)


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
