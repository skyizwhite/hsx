(defpackage #:hsx-test/element
  (:use #:cl
        #:fiveam
        #:hsx/element))
(in-package #:hsx-test/element)

(def-suite element-test)
(in-suite element-test)

(test element-class
  (is (typep (create-element :div nil nil) 'tag))
  (is (typep (create-element :html nil nil) 'html-tag))
  (is (typep (create-element :<> nil nil) 'fragment))
  (is (typep (create-element (lambda ()) nil nil) 'component))
  (signals error (create-element "div" nil nil)))

(test flatten-children
  (let* ((elm (create-element :p
                              nil
                              (list "a"
                                    nil
                                    (list "b" (list nil "c"))
                                    (cons "d" "e")))))
    (is (equal (list "a" "b" "c" "d" "e") (element-children elm)))))

(defun comp1 (&key prop children)
  (create-element :div
                  nil
                  (list prop
                        children)))

(test component-accepting-keyword-args
  (let ((elm (expand-component (create-element #'comp1
                                                '(:prop "value")
                                                (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child") (element-children elm)))))

(defun comp2 (&rest props)
  (create-element :div
                  nil
                  (list (getf props :prop)
                        (getf props :children))))

(test component-accepting-property-list
  (let ((elm (expand-component (create-element #'comp2
                                               '(:prop "value")
                                               (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child") (element-children elm)))))

(defun comp3 (&rest props &key prop children &allow-other-keys)
  (create-element :div
                  nil
                  (list prop
                        children
                        (getf props :other-key))))

(test component-accepting-keyword-args-and-property-list
  (let ((elm (expand-component (create-element #'comp3
                                               '(:prop "value" :other-key "other")
                                               (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child" "other") (element-children elm)))))
