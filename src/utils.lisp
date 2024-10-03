(defpackage #:hsx/utils
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table
                #:make-keyword
                #:symbolicate)
  (:export #:escape-html-attribute
           #:escape-html-text-content
           #:minify
           #:defgroup))
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

(defun minify (input-string)
  (with-output-to-string (out)
    (let ((previous-space-p nil))
      (loop for char across input-string do
               (cond
                 ((whitespace-p char)
                  (unless previous-space-p
                    (write-char #\Space out))
                  (setf previous-space-p t))
                 (t
                  (write-char char out)
                  (setf previous-space-p nil)))))))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Tab #\Return) :test #'char=))

(defun make-keyword-hash-table (symbols)
  (let ((ht (make-hash-table)))
    (mapcar (lambda (sym)
              (setf (gethash (make-keyword sym) ht) t))
            symbols)
    ht))

(defmacro defgroup (name &body symbols)
  (let ((param-name (symbolicate '* name '*))
        (pred-name (symbolicate name '-p)))
    `(progn
       (defparameter ,param-name (make-keyword-hash-table ',symbols))
       (defun ,pred-name (keyword)
         (gethash keyword ,param-name)))))
