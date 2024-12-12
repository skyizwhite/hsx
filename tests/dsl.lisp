(defpackage #:hsx-test/dsl
  (:use #:cl
        #:rove
        #:hsx/dsl)
  (:import-from #:hsx/builtin)
  (:import-from #:hsx/element
                #:element-props
                #:element-children))
(in-package #:hsx-test/dsl)

(deftest find-builtin-symbols-test
  (testing "normal-cases"
    (ok (expands '(hsx (div div div))
                 '(hsx/builtin:div div div)))
    (ok (expands '(hsx (div (div div (div))))
                 '(hsx/builtin:div
                   (hsx/builtin:div
                     div
                     (hsx/builtin:div)))))
    (ok (expands '(hsx (div
                         (labels ((div () "div"))
                           (hsx (div)))))
                 '(hsx/builtin:div
                   (labels ((div () "div"))
                     (hsx (div)))))))

  (testing "ignore-cases"
    (ok (expands '(hsx (div . div))
                 '(div . div)))
    (ok (expands '(hsx ((div)))
                 '((div))))
    (ok (expands '(hsx (div
                         (labels ((div () "div"))
                           (div))))
                 '(hsx/builtin:div
                   (labels ((div () "div"))
                     (div)))))))

(deftest dsl-test
  (testing "empty-hsx"
    (let ((elm (hsx (div))))
      (ok (null (element-props elm)))
      (ok (null (element-children elm)))))
 
  (testing "hsx-with-static-props"
    (let ((elm (hsx (div :prop1 "value1" :prop2 "value2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "hsx-with-dynamic-props"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (hsx (div props))))
      (ok (equal props (element-props elm)))
      (ok (null (element-children elm)))))
  
  (testing "hsx-with-children"
    (let ((elm (hsx (div
                      "child1"
                      "child2"))))
      (ok (null (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "hsx-with-static-props-and-children"
    (let ((elm (hsx (div :prop1 "value1" :prop2 "value2"
                      "child1"
                      "child2"))))
      (ok (equal '(:prop1 "value1" :prop2 "value2")
                 (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm)))))
  
  (testing "hsx-with-dynamic-props-and-children"
    (let* ((props '(:prop1 "value1" :prop2 "value2"))
           (elm (hsx (div props
                       "child1"
                       "child2"))))
      (ok (equal props (element-props elm)))
      (ok (equal (list "child1" "child2") (element-children elm))))))
