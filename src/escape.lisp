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

(defparameter *html-escape-map*
  '((#\& . "&amp;")
    (#\< . "&lt;")
    (#\> . "&gt;")
    (#\" . "&quot;")
    (#\' . "&#x27;")
    (#\/ . "&#x2F;")
    (#\` . "&grave;")
    (#\= . "&#x3D;")))

(defparameter *attr-escape-map*
  '((#\" . "&quot;")))

(defun escape-char (char escape-map)
  (or (cdr (assoc char escape-map))
       char))

(defun escape-string (string escape-map)
  (if (stringp string)
      (with-output-to-string (s)
        (loop
          for c across string
          do (write (escape-char c escape-map)
                    :stream s :escape nil)))
      string))

(defun escape-attrs-alist (alist)
  (mapcar (lambda (kv)
            (cons (car kv)
                  (escape-string (cdr kv) *attr-escape-map*)))
          alist))

(defun escape-children (children)
  (mapcar (lambda (child)
            (if (stringp child)
                (escape-string child *html-escape-map*)
                child))
          children))
