(defpackage #:piccolo/groups
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate
                #:make-keyword)
  (:export #:self-closing-tag-p
           #:non-escape-tag-p))
(in-package #:piccolo/groups)

(defun symbols-hash-table (symbols)
  (let ((ht (make-hash-table)))
    (mapcar (lambda (sym)
              (setf (gethash (make-keyword sym) ht) t))
            symbols)
    ht))

(defmacro define-group (name &body symbols)
  (with-gensyms (ht)
    `(progn
       (let ((,ht (symbols-hash-table ',symbols)))
         (defun ,(symbolicate name '-p) (symbol)
           (gethash (make-keyword (string-upcase symbol)) ,ht))))))

(define-group self-closing-tag
  area base br col embed hr img input keygen
  link meta param source track wbr)

(define-group non-escape-tag
  style script textarea pre)
