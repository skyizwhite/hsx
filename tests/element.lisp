(defpackage #:hsx-test/element
  (:use #:cl
        #:fiveam
        #:hsx/element))
(in-package #:hsx-test/element)

(def-suite element-test)
(in-suite element-test)

(test tag
  (let ((elm (create-element :p
                             '(:class "red")
                             "Hello,"
                             "World")))
    (is (eq :p (element-type elm)))
    (is (equal '(:class "red") (element-props elm)))
    (is (equal (list "Hello," "World") (element-children elm)))))

(test flatten-children
  (let* ((elm (create-element :p
                              nil
                              "a"
                              nil
                              (list "b" (list nil "c"))
                              (cons "d" "e"))))
    (is (equal (list "a" "b" "c" "d" "e") (element-children elm)))))

(defun comp1 (&key title children)
  (create-element :div
                  nil
                  title
                  children))

(test component-accepting-keyword-args
  (let* ((elm (create-element #'comp1
                              '(:title "foo")
                              "bar"))
         (expanded (expand-component elm)))
    (is (eq #'comp1 (element-type elm)))
    (is (equal '(:title "foo") (element-props elm)))
    (is (equal (list "bar") (element-children elm)))
    (is (eq :div (element-type expanded)))
    (is (equal (list "foo" "bar") (element-children expanded)))))

(defun comp2 (&rest props)
  (create-element :div
                  nil
                  (getf props :title)
                  (getf props :children)))

(test component-accepting-property-list
  (let* ((elm (create-element #'comp2
                              '(:title "foo")
                              "bar"))
         (expanded (expand-component elm)))
    (is (eq #'comp2 (element-type elm)))
    (is (equal '(:title "foo") (element-props elm)))
    (is (equal (list "bar") (element-children elm)))
    (is (eq :div (element-type expanded)))
    (is (equal (list "foo" "bar") (element-children expanded)))))

(defun comp3 (&rest props &key title children &allow-other-keys)
  (create-element :div
                  nil
                  title
                  children
                  (getf props :other-key)))

(test component-accepting-keyword-args-and-property-list
  (let* ((elm (create-element #'comp3
                              '(:title "foo" :other-key "baz")
                              "bar"))
         (expanded (expand-component elm)))
    (is (eq #'comp3 (element-type elm)))
    (is (equal '(:title "foo" :other-key "baz") (element-props elm)))
    (is (equal (list "bar") (element-children elm)))
    (is (eq :div (element-type expanded)))
    (is (equal (list "foo" "bar" "baz") (element-children expanded)))))
