(defpackage #:hsx-test/hsx
  (:use #:cl
        #:fiveam
        #:hsx/hsx)
  (:import-from #:hsx/element
                #:element-type
                #:element-children)
  (:import-from #:hsx/defhsx
                #:defcomp)
  (:import-from #:hsx/builtin))
(in-package #:hsx-test/hsx)


(def-suite hsx-test)
(in-suite hsx-test)

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
