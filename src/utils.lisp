(defpackage #:hsx/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:make-keyword
                #:symbolicate)
  (:export #:escape-html-attribute
           #:escape-html-text-content
           #:clsx))
(in-package #:hsx/utils)

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

(defun escape-string (str escape-map)
  (if (stringp str)
      (with-output-to-string (out)
        (loop
          :for c :across str
          :do (write (escape-char c escape-map) :stream out :escape nil)))
      str))

(defun escape-html-text-content (str)
  (escape-string str *text-content-escape-map*))

(defun escape-html-attribute (str)
  (escape-string str *attribute-escape-map*))

(defun clsx (&rest strs)
  (format nil "~{~a~^ ~}" (remove nil strs)))
