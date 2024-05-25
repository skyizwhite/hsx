(defpackage :hsx-test/element
  (:use :cl
        :fiveam
        :hsx/element))
(in-package :hsx-test/element)

(def-suite create-element)

(in-suite create-element)

(test create-builtin-element
  (let* ((inner (create-element "span"
                                '(:class "red")
                                "World!"))
         (outer (create-element "p"
                                nil
                                "Hello,"
                                inner)))
    (is (string= (element-kind inner) "span"))
    (is (equal (element-props inner) `(:class "red")))
    (is (equal (element-children inner) (list "World!")))
    (is (string= (element-kind outer) "p"))
    (is (null (element-props outer)))
    (is (equal (element-children outer) (list "Hello," inner)))))

(test flatten-element-children
  (let* ((elm (create-element "p"
                              nil
                              "a"
                              nil
                              (list "b" (list nil "c"))
                              (cons "d" "e"))))
    (is (equal (element-children elm) (list "a" "b" "c" "d" "e")))))

(test create-component-element
  (labels ((comp (&key variant children)
             (create-element "p"
                             `(:class ,variant)
                             "Hello,"
                             children)))
    (let* ((inner (create-element "span"
                                  nil
                                  "World!"))
           (outer (create-element #'comp
                                  '(:variant "red")
                                  inner)))
      (is (eql (element-kind outer) #'comp))
      (is (equal (element-props outer) `(:variant "red")))
      (is (equal (element-children outer) (list inner)))
      (let ((expanded-elm (expand outer)))
        (is (string= (element-kind expanded-elm) "p"))
        (is (equal (element-props expanded-elm) `(:class "red")))
        (is (equal (element-children expanded-elm) (list "Hello," inner)))))))
