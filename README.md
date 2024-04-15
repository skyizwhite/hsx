# Piccolo

Piccolo, a fork of [flute](https://github.com/ailisp/flute), is a beautiful, easily composable HTML5 generation library for Common Lisp.

It's

- Simple: the most simplistic syntax, for builtin and customized elements;
- Easy to debug: pretty print generated html snippet in REPL;
- Powerful: help you define reusable and composable components, like that in React
- Modern: focus only on HTML5

# Differences from Flute

- New features:
  - Fragment `(<> ...)`: This allows you to group elements without a wrapper element.
  - Boolean attributes support (e.g. `checked`, `disabled`): If the value is
    - `nil`: Nothing is rendered.
    - `t`: Only the key is rendered.
    - non-boolean: The key/value pair is rendered.
  - `props`: If the properties given to a component are not declared with `define-element` macro, they are gathered into `props` property list. This allows flexible props passing to components.

```lisp
(<>
  (div)
  (div))

; <div></div> 
; <div></div>

(define-element view-more ()
  (a props
    "View More"))

(view-more :href "/detail" :class "m-1")

; <a href="/detail" class="m-1">View More</a>

(define-element custom-button (variant)
  (button `(:class ,variant ,@props)
    children))

(custom-button :type "submit" :variant "big" :onclick "doSomething()"
  "Submit")

; <button class="big" type="submit" onclick="doSomething()">Submit</button>
```

- Improved:
  - Element functions are wrapped in macros for natural indentation.
  - Bugfix. https://github.com/ailisp/flute/issues/5, https://github.com/ailisp/flute/issues/7
- Removed:
  - Attributes like CSS selectors (e.g. `div#id.class`)
  - ASCII-based escaping. Piccolo only supports UTF-8.

# Getting started

## Install and run tests

```lisp
(ql:quickload :piccolo)
(ql:quickload :piccolo-test)
```

Then define a new package specifically for HTML generation, in its definition:
```lisp
(defpackage piccolo-user
  (:use :cl :piccolo))
```
If you don't want to import all symbols, see [H Macro](#h-macro), which provide a similar interface as a traditional Lisp HTML generation library.

## Using html elements
```
(html
  (head
    (link :rel "...")
    (script :src "..." :defer t))
  (body
    (div :id "a" :class "b"
      (p :style "color: red"
        "Some text")
      "Some text in div"
      (img :src "/img/dog.png")
      (a '(:href "/cat")
        (img '((:src . "/img/cat.png")))))))
```

These `html`, `div`, etc. are just functions. Element attribute can be given inline as the above example, or as alist/plist/attrs object as the first argument, like the last `a` and `img` in the above example. In this case they can be variables that calculated programmatically.

The remaining argument will be recognized as the children of this element. Each child can be:
1. string;
2. element, builtin or user defined;
3. list of 1, 2 and 3. Can also be NIL.
All children will be flattened as if they're given inline.

## Define new element
```lisp
(define-element dog (id size)
  (if (and (realp size) (> size 10))
      (div :id id :class "big-dog"
              children
              "dog")
      (div :id id :class "small-dog"
              children
              "dog")))
```
`dog` will be defined as a function that takes `:id` and `:size` keyword arguments. `dog` returns an user-defined element object. Inside it, `children` will be replaced with the children elements you provided when creating this `dog`:
```
piccolo-USER> (defparameter *dog1* (dog :id "dog1" :size 20))
*DOG1*
piccolo-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
piccolo-USER> (dog :id "dog2" "I am a dog" *)
<div id="dog2" class="small-dog">
  I am a dog
  <div id="dog1" class="big-dog">dog</div>
  dog
</div>
```

All elements, both builtin and user defined ones are objects, although they're printed as html snippet in REPL. Their attribute can be accessed by `(element-attrs element)`. Their children can be accessed by `(element-children elements)` and tag name by `(element-tag element)`. You can modify an exising element's attrs and children. If you modify a user defined element, the body you defined in it's `define-element` also re-executed to take effect of the the attrs and children change:
```
piccolo-USER> *dog1*
<div id="dog1" class="big-dog">dog</div>
piccolo-USER> (setf (attr *dog1* :size) 10
                  ;; attr is a helper method to set (piccolo:element-attrs *dog1*)
                  (attr *dog1* :id) "dooooog1"
                  (element-children *dog1*) (list "i'm small now"))
("i'm small now")
piccolo-USER> *dog1*
<div id="dooooog1" class="small-dog">
  i'm small now
  dog
</div>
```

By default user element is printed as what it expand to. If you have a lot of user defined element nested deeply, you probably want to have a look at the high level:
```
piccolo-USER> (let ((*expand-user-element* nil))
              (print *dog1*)
              (values))

<dog id="dooooog1" size=10>i'm small now</dog>
; No value
piccolo-USER>
```

## Generate HTML
To generate a piece of HTML string that probably used in a response of a backend service:
```lisp
(elem-str element)
```
To generate HTML string that has nice indent as that in REPL:
```lisp
(element-string element)
```
To generate that and write to file, just create a stream, then `(write element :stream stream)` for human or `(write element :stream stream :pretty nil)` for production.

## H macro
If you don't want to import all the symbols, you can use the `h` macro:
```lisp
(defpackage piccolo-min
  (:use :cl)
  (:import-from :piccolo
                :h
                :define-element))
```
Then just wrap `h` for all html generation part. In the same examples above, it becomes:
``` lisp
(in-package :piccolo-min)
(h (html
     (head
       (link :rel "...")
       (script :src "..."))
     (body
       (div :id "a" :class "b"
         (p :style "color: red"
           "Some text")
         "Some text in div"
         (img :src "/img/dog.png")
         (a '(:href "/cat")
           (img '((:src . "/img/cat.png"))))))))

(define-element dog (id size)
  (if (and (realp size) (> size 10))
      (h (div :id id :class "big-dog"
              piccolo:children
              "dog"))
      (h (div :id id :class "small-dog"
              piccolo:children
              "dog"))))

(defparameter *dog2* (dog :id "dog2" :size 20 "some children"))
```

That's all you need to know to define elements and generate html. Please reference the [API Reference](#api-reference) Section for detailed API.

# Motivation
Currently there're a few HTML generation library in Common Lisp, like [CL-WHO](https://edicl.github.io/cl-who/), [CL-MARKUP](https://github.com/arielnetworks/cl-markup) and [Spinneret](https://github.com/ruricolist/spinneret). They both have good features for generating standard HTML, but not very good at user element (components) that currently widely used in frontend: you need to define all of them as macros and to define components on top of these components, you'll have to make these components more complex macros to composite them. [Spinneret](https://github.com/ruricolist/spinneret) has a `deftag` feature, but `deftag` is still expand to a `defmacro`.

I'd also want to modify the customer component attribute after create it and incorporate it with it's own logic (like the dog size example above), this logic should be any lisp code. This requires provide all element as object, not plain HTML text generation. With this approach, all elements have a same name function to create it, and returns element that you can modify later. These objects are virtual doms and it's very pleasant to write html code and frontend component by just composite element objects as arguments in element creation function calls. piccolo's composite feature inspired by [Hiccup](https://github.com/weavejester/hiccup) and [Reagent](https://github.com/reagent-project/reagent) but more powerful -- in piccolo, user defined elements is real object with attributes and it's own generation logic.

# API Reference
Here is a draft version of API Reference, draft means it will be better organized and moved to a separate HTML doc, but it's content is already quite complete.

## Builtin HTML elements
```
    a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup data datalist
    dd del details dfn dialog div dl dt em embed fieldset figcaption
    figure footer form h1 h2 h3 h4 h5 h6 head header hr i iframe html
    img input ins kbd label legend li link main |map| mark meta meter nav
    noscript object ol optgroup option output p param picture pre progress
    q rp rt ruby s samp script section select small source span strong
    style sub summary sup svg table tbody td template textarea tfoot th
    thead |time| title tr track u ul var video wbr
```
All of above HTML5 elements are functions, which support same kinds of parameters, take `A` as example:
``` lisp
;; Function A &REST ATTRS-AND-CHILREN
;;
;; Create and return an <a> element object
;; ATTRS-AND-CHILDREN can be the following:

;; 1. an empty <a> tag
(a)

;; 2. attributes of alist, plist or ATTRS object
;; The following creates: <a id="aa" customer-attr="bb"></a>
(a :id "aa" :customer-attr "bb")
(a '(:id "aa" :customer-attr "bb"))
(a '((:id . "aa") (:customer-attr . "bb")))
;; or assume we have the above one in variable a1
(a (element-attrs a1)) ; to share the same attrs with a1
(a (copy-attrs (element-attrs a1)))

;; 3. any of above format attributes with children
(a :id "aa" :customer-attr "bb"
  "Some children"
  (div '(:id "an element children"))
  ; list of any depth containing elements and texts, will be flattened
  (list a1 a2 (a '((:id . "aaa")) "some text")
        (list (h1 "aaa")))
  "some other text")
```
The `HTML` element is a little special, it's with `<!DOCTYPE html>` prefix to make sure browser recognize it correctly.

## User defined elements
```lisp
;; Macro DEFINE-ELEMENT NAME (&REST ARGS) &BODY BODY
;;
;; Define a user element with NAME as its tag name and function
;; NAME. After DEFINE-ELEMENT, a FUNCTION of NAME in current package
;; is defined. ARGS specified the possible keyword ARGS it can take as
;; it's ATTRS. You can either use these ARGS as Lisp arguments in the
;; BODY of its definition and plug in them to the BODY it expand to.
;; You can use piccolo:CHILDREN to get or set it's children that you give
;; when call function NAME, piccolo:ATTRS to get or set it's attributes
;; and piccolo:TAG to get or set it's tag name.

;; Variable *EXPAND-USER-ELEMENT*
;;
;; Bind this variable to specify whether the user elements are print in
;; a high level (NIL), or expand to HTML elements (T). T by default.
```

## Attribute accessing utility
``` lisp
;; Function ATTRS-ALIST ATTRS
;; Function (SETF ATTRS-ALIST) ATTRS
;;
;; Return or set the attrs object in alist format

;; Function MAKE-ATTRS &KEYS ALIST
;;
;; Create a attrs aoject, given an alist of (:attr . "attr-value") pair.
;; Attribute values (cdr of each element in alist) will be escaped if
;; *ESCAPE-HTML* is t.

;; Function COPY-ATTRS ATTRS
;;
;; Make a copy and return the copy of ATTRS object

;; Method ATTR ATTRS KEY
;; Method (SETF ATTR) ATTRS KEY
;; Method ATTR ELEMENT KEY
;; Method (SETF ATTR) ELEMENT KEY
;;
;; Get or set the attribute value of given KEY. KEY should be an keyword.
;; If KEY does not exist, ATTR method will return NIL. (SETF ATTR) method
;; will create the (KEY . VALUE) pair. Don't use (SETF (ATTR ATTRS :key) NIL)
;; or (SETF (ATTR ELEMENT :key) NIL) to remove an attr, use DELETE-ATTR.

;; Method DELETE-ATTR ATTRS KEY
;; Method DELETE-ATTR ELEMENT KEY
;;
;; Delete the attribute key value pair from ATTRS or ELEMENT's ELEMENT-ATTRS,
;; will ignore if KEY doesn't exist.

```

## Element slots
```lisp
;; Method ELEMENT-TAG ELEMENT
;; Method (SETF ELEMENT-TAG) ELEMENT
;;
;; Get or set the ELEMENT-TAG STRING. For example <html>'s ELEMENT-TAG is "html"

;; Method ELEMENT-ATTRS ELEMENT
;; Method (SETF ELEMENT-ATTRS) ELEMENT
;;
;; Get or set the ELEMENT-ATTRS. When set this, must be an ATTRS object

;; Method ELEMENT-CHILDREN ELEMENT
;; Method (SETF ELEMENT-CHILDREN) ELEMENT
;;
;; Get or set element children. When set this manually, must given a flatten list
;; of ELEMENT or STRING.

;; Method USER-ELEMENT-EXPAND-TO USER-ELEMENT
;;
;; Get what this USER-ELEMENT-TO. Returns the root ELEMENT after it expands.
```

## The H macro
```lisp
;; Macro H &BODY CHILDREN
;;
;; Like a PROGN, except it will replace all html tag SYMBOLs with the same name one
;; in piccolo PACKAGE, so you don't need to import all of them. As an alternative you
;; can import all or part of html element functions in piccolo PACKAGE to use them
;; without H macro

```

## Escape utility
```lisp
;; Variable *ESCAPE-HTML*
;;
;; Specify the escape option when generate html with UTF-8, can be t or NIL.
;; If t, escape only #\<, #\> and #\& in body, and \" in attribute keys. #\' will
;; in attribute keys will not be escaped since piccolo will always use double quote for
;; attribute keys.
;; If NIL, nothing is escaped and programmer is responsible to escape elements properly.
;; All the escapes are done in element creation time.

;; Function ESCAPE-STRING STRING TEST
;;
;; Escape the STRING if it's a STRING and escaping all charaters C that satisfied
;; (FUNCALL TEST C). Return the new STRING after escape.
```

## Generate HTML string

``` lisp
;; Method ELEMENT-STRING ELEMENT
;;
;; Return human readable, indented HTML string for ELEMENT

;; Method ELEM-STR ELEMENT
;;
;; Return minify HTML string for ELEMENT
```


# License
Licensed under MIT License.　

Copyright (c) 2024, skyizwhite.

Copyright (c) 2018, Bo Yao. All rights reserved.
