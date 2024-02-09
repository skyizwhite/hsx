(uiop:define-package #:piccolo/util
  (:use #:cl)
  (:export
   ;;; list utility
   #:plist-alist

   ;;; escape utility
   #:*escape-html*
   #:utf8-html-escape-char-p
   #:ascii-html-escape-char-p
   #:attr-value-escape-char-p
   #:escape-string
   #:escape-attrs-alist
   #:escape-children

   ;;; syntax tree utility
   #:modify-first-leaves))
(in-package #:piccolo/util)

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons k v)))

(defparameter *escape-html* :utf8
  "Specify the escape option when generate html, can be :UTF8, :ASCII, :ATTR or NIL.
If :UTF8, escape only #\<, #\> and #\& in body, and \" in attribute keys. #\' will
in attribute keys will not be escaped since piccolo will always use double quote for
attribute keys.
If :ASCII, besides what escaped in :UTF8, also escape all non-ascii characters.
If :ATTR, only #\" in attribute values will be escaped.
If NIL, nothing is escaped and programmer is responsible to escape elements properly.
When given :ASCII and :ATTR, it's possible to insert html text as a children, e.g.
(div :id \"container\" \"Some <b>text</b>\")")

(defun utf8-html-escape-char-p (char)
  (find char "<>&"))

(defun ascii-html-escape-char-p (char)
  (or (utf8-html-escape-char-p char)
      (> (char-code char) 127)))

(defun attr-value-escape-char-p (char)
  (eql char #\"))

(defun escape-char (char)
  (case char
    (#\< "&lt;")
    (#\> "&gt;")
    (#\& "&amp;")
    (#\' "&#039;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape-string (string &optional (test #'utf8-html-escape-char-p))
  (if (stringp string)
      (with-output-to-string (s)
        (loop
          for c across string
          do (write (if (funcall test c) (escape-char c) c) :stream s :escape nil)))
      string))

(defun escape-attrs-alist (alist)
  (mapcar (lambda (kv)
            (cons (car kv)
                  (escape-string (cdr kv) #'attr-value-escape-char-p)))
          alist))

(defun escape-children (children)
  (mapcar (lambda (child)
            (if (stringp child)
                (case *escape-html*
                  (:utf8 (escape-string child))
                  (:ascii (escape-string child #'ascii-html-escape-char-p))
                  (otherwise child))
                child))
          children))

(defun modify-first-leaves (tree test result)
  (if tree
      (cons (let ((first-node (first tree)))
              (cond
                ((listp first-node)
                 (modify-first-leaves first-node test result))
                ((funcall test first-node)
                 (funcall result first-node))
                (t first-node)))
            (mapcar (lambda (node)
                      (if (listp node)
                          (modify-first-leaves node test result)
                          node))
                    (rest tree)))))
