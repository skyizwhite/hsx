(defpackage :hsx-test/element
  (:use :cl
        :fiveam
        :hsx/element))
(in-package :hsx-test/element)

(def-suite create-element)

(in-suite create-element)

(test create-html-element
  (let* ((inner (create-element "span"
                                '(:class "red")
                                "World!"))
         (outer (create-element "p"
                                nil
                                "Hello,"
                                inner)))
    (with-accessors ((kind element-kind)
                     (props element-props)) inner
      (is (string= kind "span"))
      (is (equal props `(:class "red" :children ("World!")))))
    (with-accessors ((kind element-kind)
                     (props element-props)) outer
      (is (string= kind "p"))
      (is (equal props `(:children ("Hello," ,inner)))))))

(test flatten-element-children
  (let* ((elm (create-element "p"
                              nil
                              "a"
                              nil
                              (list "b" (list nil "c"))
                              (cons "d" "e")))
         (children (getf (element-props elm) :children)))
    (is (equal children (list "a" "b" "c" "d" "e")))))

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
      (with-accessors ((kind element-kind)
                       (props element-props)) outer
        (is (eql kind #'comp))
        (is (equal props `(:variant "red" :children (,inner)))))
      (with-accessors ((kind element-kind)
                       (props element-props)) (expand outer)
        (is (string= kind "p"))
        (is (equal props `(:class "red" :children ("Hello," ,inner))))))))
