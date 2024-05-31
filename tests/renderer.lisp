(defpackage #:hsx-test/renderer
  (:use #:cl
        #:fiveam
        #:named-readtables
        #:hsx/builtin)
  (:import-from #:mstrings)
  (:import-from #:hsx/element
                #:render))
(in-package :hsx-test/renderer)
(in-readtable mstrings:mstring-syntax)

(def-suite renderer-test)
(in-suite renderer-test)

(test empty-tag
  (is (string= "<div></div>"
               (render (div)))))

(test tag-with-props
  (is (string= "<div prop1=\"value1\" prop2></div>"
               (render (div :prop1 "value1" :prop2 t :prop3 nil)))))

(test tag-with-children
  (is (string= "<p>foo</p>"
               (render (p "foo"))))
  (is (string= #M"<p>
                 \  <span>foo</span>
                 \</p>"
               (render (p
                         (span "foo")))))
  (is (string= #M"<p>
                 \  foo
                 \  <span>bar</span>
                 \</p>"
               (render (p
                         "foo"
                         (span "bar"))))))

(test tag-with-props-and-children
  (is (string=  "<p prop1=\"value1\" prop2>foo</p>"
                (render (p :prop1 "value1" :prop2 t :prop3 nil
                          "foo"))))
  (is (string= #M"<p prop1=\"value1\" prop2>
                 \  foo
                 \  <span>bar</span>
                 \</p>"
               (render (p :prop1 "value1" :prop2 t :prop3 nil
                         "foo"
                         (span "bar"))))))

(test fragment
  (let ((frg (<>
               (li "bar")
               (li "baz"))))
    (is (string= #M"<li>bar</li>
                    <li>baz</li>"
                 (render frg)))
    (is (string= #M"<ul>
                   \  <li>foo</li>
                   \  <li>bar</li>
                   \  <li>baz</li>
                   \  <li>brah</li>
                   \</ul>"
                 (render (ul
                           (li "foo")
                           frg
                           (li "brah")))))))
