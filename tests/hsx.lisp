(defpackage #:hsx-test/hsx
  (:use #:cl
        #:fiveam
        #:hsx/hsx))
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
