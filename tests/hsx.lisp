(defpackage #:hsx-test/hsx
  (:use #:cl
        #:fiveam
        #:hsx/hsx
        #:hsx/builtin)
  (:import-from #:hsx/element
                #:element-props
                #:element-children))
(in-package #:hsx-test/hsx)

(def-suite hsx-test)
(in-suite hsx-test)

(test find-symbols
  (is (equal '(hsx/builtin:div '(:div "div")
               div
               (hsx/builtin:div
                 'div
                 (hsx/builtin:div)
                 :div)
               "div")
             (macroexpand-1
              '(hsx (div '(:div "div")
                      div
                      (div
                        'div
                        (div)
                        :div)
                      "div"))))))

(test empty-hsx
  (let ((elm (div)))
    (is (null (element-props elm)))
    (is (null (element-children elm)))))

(test hsx-with-static-props
  (let ((elm (div :prop1 "value1" :prop2 "value2")))
    (is (equal '(:prop1 "value1" :prop2 "value2")
               (element-props elm)))
    (is (null (element-children elm)))))

(test hsx-with-dynamic-props
  (let* ((props '(:prop1 "value1" :prop2 "value2"))
         (elm (div props)))
    (is (equal props (element-props elm)))
    (is (null (element-children elm)))))

(test hsx-with-children
  (let ((elm (div
               "child1"
               "child2")))
    (is (null (element-props elm)))
    (is (equal (list "child1" "child2") (element-children elm)))))

(test hsx-with-static-props-and-children
  (let ((elm (div :prop1 "value1" :prop2 "value2"
               "child1"
               "child2")))
    (is (equal '(:prop1 "value1" :prop2 "value2")
               (element-props elm)))
    (is (equal (list "child1" "child2") (element-children elm)))))

(test hsx-with-dynamic-props-and-children
  (let* ((props '(:prop1 "value1" :prop2 "value2"))
         (elm (div props
                "child1"
                "child2")))
    (is (equal props (element-props elm)))
    (is (equal (list "child1" "child2") (element-children elm)))))
