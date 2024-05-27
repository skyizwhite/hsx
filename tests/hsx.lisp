(defpackage #:hsx-test/hsx
  (:use #:cl
        #:fiveam
        #:hsx/hsx
        #:hsx/builtin)
  (:import-from #:hsx/element
                #:create-element))
(in-package #:hsx-test/hsx)

(def-suite hsx-test)
(in-suite hsx-test)

(test empty-hsx
  (is (equal (macroexpand-1
              '(div))
             '(create-element
               "div"
               (list)))))

(test hsx-with-props
  (is (equal (macroexpand-1
              '(div :prop1 "value1" :prop2 "value2"))
             '(create-element
               "div"
               (list :prop1 "value1" :prop2 "value2")))))

(test hsx-with-children
  (is (equal (macroexpand-1
              '(div
                "child1"
                "child2"))
             '(create-element
               "div"
               (list)
               "child1"
               "child2"))))

(test hsx-with-props-and-children
  (is (equal (macroexpand-1
              '(div :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               "div"
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))

(defhsx custom "custom")

(test hsx-for-custom-tag-element
  (is (equal (macroexpand-1
              '(custom :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               "custom"
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))

(defhsx comp1 #'%comp1)
(defun %comp1 (&key prop1 prop2 children)
  (declare (ignore prop1 prop2 children)))

(defcomp comp2 (&key prop1 prop2 children)
  (declare (ignore prop1 prop2 children)))

(test hsx-for-component-element
  (is (equal (macroexpand-1
              '(comp1 :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               #'%comp1
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2")))
  (is (equal (macroexpand-1
              '(comp2 :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               (fdefinition '%comp2)
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))
