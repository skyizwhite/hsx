(defpackage #:hsx/escaper
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:export #:escape-html-attribute
           #:escape-html-text-content))
(in-package #:hsx/escaper)

(defparameter *text-content-escape-map*
  (alist-hash-table
   '((#\& . "&amp;")
     (#\< . "&lt;")
     (#\> . "&gt;")
     (#\" . "&quot;")
     (#\' . "&#x27;")
     (#\/ . "&#x2F;")
     (#\` . "&grave;")
     (#\= . "&#x3D;"))))

(defparameter *attribute-escape-map*
  (alist-hash-table
   '((#\" . "&quot;"))))

(defun escape-char (char escape-map)
  (or (gethash char escape-map) 
      char))

(defun escape-string (string escape-map)
  (if (stringp string)
      (with-output-to-string (s)
        (loop
          :for c :across string
          :do (write (escape-char c escape-map) :stream s :escape nil)))
      string))

(defun escape-html-text-content (text)
  (escape-string text *text-content-escape-map*))

(defun escape-html-attribute (text)
  (escape-string text *attribute-escape-map*))
