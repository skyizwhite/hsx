(defpackage #:hsx-test/hsx-macro
  (:use #:cl
        #:fiveam)
  (:import-from #:hsx/element
                #:element-type
                #:element-props)
  (:import-from #:hsx/hsx
                #:hsx
                #:defcomp))
(in-package #:hsx-test/hsx-macro)

(def-suite hsx-macro-test)
(in-suite hsx-macro-test)

(defcomp div (&rest props)
  (declare (ignore props))
  "This is fake!")

(defcomp p (&rest props)
  (declare (ignore props))
  "This is fake!")

(test find-symbols
  (let ((fake-elm (div :prop "value"
                    (p "brah"))))
    (is (eql (element-type fake-elm) #'%div)
        (eql (element-type (first (element-children fake-elm))) #'%p)))
  (let ((true-elm (hsx (div :prop "value"
                         (p "brah")))))
    (is (equal (element-type true-elm) "div")
        (equal (element-type (first (element-children true-elm))) "p"))))
