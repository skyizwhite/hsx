(defpackage :hsx-test/element
  (:use :cl
        :fiveam
        :hsx/element))
(in-package :hsx-test/element)

(def-suite element-test)
(in-suite element-test)

(test tag-element
  (let ((elm (create-element "p"
                               '(:class "red")
                               "Hello,"
                               "World")))
    (is (string= (element-type elm) "p"))
    (is (equal (element-props elm) '(:class "red")))
    (is (equal (element-children elm) (list "Hello," "World")))))

(test flatten-children
  (let* ((elm (create-element "p"
                              nil
                              "a"
                              nil
                              (list "b" (list nil "c"))
                              (cons "d" "e"))))
    (is (equal (element-children elm) (list "a" "b" "c" "d" "e")))))

(defun comp1 (&key title children)
  (create-element "div"
                  nil
                  title
                  children))

(test component-elment-with-keyword-args
  (let* ((elm (create-element #'comp1
                              '(:title "foo")
                              "bar"))
         (expanded (expand elm)))
    (is (eql (element-type elm) #'comp1))
    (is (equal (element-props elm) '(:title "foo")))
    (is (equal (element-children elm) (list "bar")))
    (is (string= (element-type expanded) "div"))
    (is (equal (element-children expanded) (list "foo" "bar")))
    (signals error
      (create-element #'comp1
                      '(:title "foo" :other-key "baz")
                      "bar"))))

(defun comp2 (&rest props)
  (create-element "div"
                  nil
                  (getf props :title)
                  (getf props :children)))

(test component-element-with-property-list
  (let* ((elm (create-element #'comp2 
                              '(:title "foo")
                              "bar"))
         (expanded (expand elm)))
    (is (eql (element-type elm) #'comp2))
    (is (equal (element-props elm) '(:title "foo")))
    (is (equal (element-children elm) (list "bar")))
    (is (string= (element-type expanded) "div"))
    (is (equal (element-children expanded) (list "foo" "bar")))))

(defun comp3 (&rest props &key title children &allow-other-keys)
  (create-element "div"
                  nil
                  title
                  children
                  (getf props :other-key)))

(defun comp4 (&rest props &key title children)
  (create-element "div"
                  nil
                  title
                  children
                  (getf props :other-key)))

(test component-element-with-keyword-args-and-property-list
  (let* ((elm (create-element #'comp3
                              '(:title "foo" :other-key "baz")
                              "bar"))
         (expanded (expand elm)))
    (is (eql (element-type elm) #'comp3))
    (is (equal (element-props elm) '(:title "foo" :other-key "baz")))
    (is (equal (element-children elm) (list "bar")))
    (is (string= (element-type expanded) "div"))
    (is (equal (element-children expanded) (list "foo" "bar" "baz")))
    (signals error
      (create-element #'comp4
                      '(:title "foo" :other-key "baz")
                      "bar"))))
