(defpackage #:hsx-test/utils
  (:use #:cl
        #:rove
        #:hsx/utils))
(in-package #:hsx-test/utils)

(deftest text-util-test
  (testing "escape-html-attribute"
    (ok (string= "&quot;foo&quot;"
                 (escape-html-attribute "\"foo\"")))
    (ok (string= "AT&amp;amp;T <b>"
                 (escape-html-attribute "AT&amp;T <b>"))
        "& is escaped so a literal entity survives the browser; < > are left alone")
    (ok (string= "open &amp;&amp; a < b"
                 (escape-html-attribute "open && a < b"))
        "Alpine expressions stay readable apart from &"))
  
  (testing "escape-html-text-content"
    (ok (string= "&amp;&lt;&gt;&quot;&#x27;&#x2F;&grave;&#x3D;"
                 (escape-html-text-content "&<>\"'/`="))))

  (testing "escape-html-attribute under *print-readably*"
    (ok (string= "en"
                 (let ((*print-readably* t))
                   (escape-html-attribute "en"))))))
