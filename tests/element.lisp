(defpackage :piccolo-test/element
  (:use :cl
        :fiveam
        :piccolo/element))
(in-package :piccolo-test/element)

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
