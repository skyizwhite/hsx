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
           "div")))))
