(defpackage #:hsx-test/renderer
  (:use #:cl
        #:fiveam
        #:hsx
        #:named-readtables)
  (:import-from #:mstrings))
(in-package :hsx-test/renderer)
(in-readtable mstrings:mstring-syntax)

(def-suite renderer)
(in-suite renderer)

(test empty-tag
  (is (string= "<div></div>"
               (render (hsx (div))))))

(test tag-with-props
  (is (string= "<div prop1=\"value1\" prop2></div>"
               (render (hsx (div :prop1 "value1" :prop2 t :prop3 nil))))))

(test tag-with-children
  (is (string= "<p>Hello, World!</p>"
               (render (hsx (p "Hello, World!")))))
  (is (string= #M"<p>
                 \  Hello,
                 \  <span>World!</span>
                 \</p>"
               (render (hsx (p
                              "Hello,"
                              (span "World!")))))))

(test tag-with-props-and-children
  (is (string=  "<p prop1=\"value1\" prop2>Hello, World!</p>"
                (render (hsx
                          (p :prop1 "value1" :prop2 t :prop3 nil
                            "Hello, World!")))))
  (is (string= #M"<p prop1=\"value1\" prop2>
                 \  Hello,
                 \  <span>World!</span>
                 \</p>"
               (render (hsx
                         (p :prop1 "value1" :prop2 t :prop3 nil
                           "Hello,"
                           (span "World!")))))))

(test fragment
  (let ((frg (hsx
               (<>
                 (li "bar")
                 (li "baz")))))
    (is (string= #M"<li>bar</li>
                    <li>baz</li>"
                 (render frg)))
    (is (string= #M"<ul>
                   \  <li>foo</li>
                   \  <li>bar</li>
                   \  <li>baz</li>
                   \  <li>brah</li>
                   \</ul>"
                 (render (hsx
                           (ul
                             (li "foo")
                             frg
                             (li "brah"))))))))

;; TODO: Add escaping test
