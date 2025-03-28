(defpackage #:hsx-test/utils
  (:use #:cl
        #:rove
        #:hsx/utils))
(in-package #:hsx-test/utils)

(deftest text-util-test
  (testing "escape-html-attribute"
    (ok (string= "&quot;foo&quot;"
                 (escape-html-attribute "\"foo\""))))
  
  (testing "escape-html-text-content"
    (ok (string= "&amp;&lt;&gt;&quot;&#x27;&#x2F;&grave;&#x3D;"
                 (escape-html-text-content "&<>\"'/`=")))))
