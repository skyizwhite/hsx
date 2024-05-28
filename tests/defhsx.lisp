(defpackage #:hsx-test/defhsx
  (:use #:cl
        #:fiveam
        #:hsx/defhsx
        #:hsx/builtin)
  (:import-from #:hsx/element
                #:create-element))
(in-package #:hsx-test/defhsx)


(def-suite defhsx-test)
(in-suite defhsx-test)

(test empty-hsx
  (is (equal (macroexpand-1
              '(div))
             '(create-element
               :div
               (list)))))

(test hsx-with-props
  (is (equal (macroexpand-1
              '(div :prop1 "value1" :prop2 "value2"))
             '(create-element
               :div
               (list :prop1 "value1" :prop2 "value2")))))

(test hsx-with-children
  (is (equal (macroexpand-1
              '(div
                "child1"
                "child2"))
             '(create-element
               :div
               (list)
               "child1"
               "child2"))))

(test hsx-with-props-and-children
  (is (equal (macroexpand-1
              '(div :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               :div
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))

(deftag custom)

(test hsx-for-custom-tag-element
  (is (equal (macroexpand-1
              '(custom :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               :custom
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))

(defcomp comp (&key prop1 prop2 children)
  (declare (ignore prop1 prop2 children)))

(test hsx-for-component-element
  (is (equal (macroexpand-1
              '(comp :prop1 "value1" :prop2 "value2"
                "child1"
                "child2"))
             '(create-element
               (fdefinition '%comp)
               (list :prop1 "value1" :prop2 "value2")
               "child1"
               "child2"))))