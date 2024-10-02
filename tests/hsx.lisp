(defpackage #:hsx-test/hsx
  (:use #:cl
        #:rove
        #:hsx/hsx
        #:hsx/builtin)
  (:import-from #:hsx/element
                #:element-props
                #:element-children))
(in-package #:hsx-test/hsx)

(deftest hsx-test
  (testing "find-symbols"
    (ok (expands 
         '(hsx (div '(:div "div")
                 div
                 (div
                   'div
                   (div)
                   :div)
                 "div"))
         '(hsx/builtin:div '(:div "div")
           div
           (hsx/builtin:div
             'div
             (hsx/builtin:div)
             :div)
           "div"))))
  
  (testing "empty-hsx"
    (let ((elm (div)))
      (ok (null (element-props elm)))
      (ok (null (element-children elm)))))
 
  (testing "hsx-with-static-props"
    (let ((elm (div :prop1 "value1" :prop2 "value2")))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "hsx-with-dynamic-props"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (div props)))
      (ok (equal props (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "hsx-with-children"
    (let ((elm (div
                 "child1"
                 "child2")))
      (ok (null (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "hsx-with-static-props-and-children"
    (let ((elm (div :prop1 "value1" :prop2 "value2"
                 "child1"
                 "child2")))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "hsx-with-dynamic-props-and-children"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (div props
                  "child1"
                  "child2")))
      (ok (equal props (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm))))))
