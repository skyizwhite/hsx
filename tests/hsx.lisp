(defpackage #:hsx-test/hsx
  (:use #:cl
        #:fiveam
        #:hsx/element
        #:hsx/hsx))
(in-package #:hsx-test/hsx)

(def-suite builtin-element-hsx)
(def-suite component-element-hsx)
(in-suite builtin-element-hsx)

(test empty-hsx
  (let ((elm (div)))
    (is (null (element-props elm)))
    (is (null (element-children elm)))))

(test hsx-with-props
  (let ((elm (div :prop1 "value1" :prop2 "value2")))
    (is (equal (element-props elm) '(:prop1 "value1" :prop2 "value2")))
    (is (null (element-children elm)))))

(test hsx-with-children
  (let ((elm (div "child1" "child2")))
    (is (null (element-props elm)))
    (is (equal (element-children elm) (list "child1" "child2")))))

(test hsx-with-props-and-children
  (test hsx-with-props
    (let ((elm (div :prop1 "value1" :prop2 "value2"
                 "child1" "child2")))
      (is (equal (element-props elm) '(:prop1 "value1" :prop2 "value2")))
      (is (equal (element-children elm) (list "child1" "child2"))))))
