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
               (escape-html-text-content "&<>\"'/`="))))
