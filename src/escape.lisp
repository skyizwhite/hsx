(uiop:define-package #:piccolo/escape
  (:use #:cl)
  (:export #:*escape-html*
           #:html-escape-char-p
           #:attr-value-escape-char-p
           #:escape-string
           #:escape-attrs-alist
           #:escape-children))
(in-package #:piccolo/escape)

(defparameter *escape-html* t)

(defun html-escape-char-p (char)
  (find char "<>&"))

(defun attr-value-escape-char-p (char)
  (eql char #\"))

(defun escape-char (char)
  (case char
    (#\< "&lt;")
    (#\> "&gt;")
    (#\& "&amp;")
    (#\" "&quot;")
    (t   (error "Escaped character is undefined: ~a" char))))

(defun escape-string (string test)
  (if (stringp string)
      (with-output-to-string (s)
        (loop
          for c across string
          do (write (if (funcall test c)
                        (escape-char c)
                        c)
                    :stream s :escape nil)))
      string))

(defun escape-attrs-alist (alist)
  (mapcar (lambda (kv)
            (cons (car kv)
                  (escape-string (cdr kv) #'attr-value-escape-char-p)))
          alist))

(defun escape-children (children)
  (mapcar (lambda (child)
            (if (stringp child)
                (escape-string child #'html-escape-char-p)
                child))
          children))
