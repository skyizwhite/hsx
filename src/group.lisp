(defpackage #:hsx/group
  (:use #:cl)
  (:import-from #:alexandria
                #:make-keyword
                #:symbolicate)
  (:export #:defgroup
           #:self-closing-tag-p
           #:non-escaping-tag-p))
(in-package #:hsx/group)

(defun make-keyword-hash-table (symbols)
  (let ((ht (make-hash-table)))
    (mapcar (lambda (sym)
              (setf (gethash (make-keyword sym) ht) t))
            symbols)
    ht))

(defmacro defgroup (name &body symbols)
  (let ((p-name (symbolicate '* name '*)))
    `(progn
       (defparameter ,p-name (make-keyword-hash-table ',symbols))
       (defun ,(symbolicate name '-p) (keyword)
         (gethash keyword ,p-name)))))

(defgroup self-closing-tag
  area base br col embed hr img input
  link meta param source track wbr)

(defgroup non-escaping-tag
  script style)
