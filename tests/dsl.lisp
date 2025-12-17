(defpackage #:hsx-test/dsl
  (:use #:cl
        #:rove
        #:hsx/dsl)
  (:import-from #:hsx/builtin)
  (:import-from #:hsx/web-components)
  (:import-from #:hsx/element
                #:element-props
                #:element-children))
(in-package #:hsx-test/dsl)

(defcomp ~comp1 (&key children)
  (hsx (div children)))

(deftest detect-elements-test
  (testing "detect-tags"
    (ok (expands '(hsx (div div div))
                 '(hsx/builtin:div div div)))
    (ok (expands '(hsx (div (div div (div))))
                 '(hsx/builtin:div
                   (hsx/builtin:div
                     div
                     (hsx/builtin:div))))))

  (testing "detect-components"
    (ok (expands '(hsx (~comp1 (div)))
                 '(~comp1 (hsx/builtin:div)))))

  (testing "ignore-malformed-form"
    (ok (expands '(hsx (div . div))
                 '(div . div)))
    (ok (expands '(hsx ((div)))
                 '((div)))))

  (testing "ignore-cl-form"
    (ok (expands '(hsx (labels ((div () "div"))
                         (div)))
                 '(labels ((div () "div"))
                   (div))))))

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

(register-web-components c1 c2)

(deftest web-components-test
  (testing "register-web-components"
    (ok (expands '(hsx (c1 (c2)))
                 '(hsx/web-components:c1 (hsx/web-components:c2)))))
  (testing "clear-web-components"
    (clear-web-components)
    (ok (expands '(hsx (c1 (c2)))
                 '(c1 (c2))))))
