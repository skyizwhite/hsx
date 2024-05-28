# HSX

⚠️ This project is a work in progress.

HSX (Hypertext S-expression) is a straightforward HTML5 generation library for Common Lisp.

This project is a fork of [flute](https://github.com/ailisp/flute/), originally created by Bo Yao.

## Usage

With the `hsx` macro, you can construct HTML using S-expressions.

```lisp
(hsx
  (div :id "greeting" :class "flex"
    (h1 "Hello World")
    (p
      "This is"
      (strong "an example!"))))
```

This code generates the following HTML:

```html
<div id="greeting" class="flex">
  <h1>Hello World</h1>
  <p>
    This is
    <strong>an example!</strong>
  </p>
</div>
```

HSX elements are essentially functions, allowing you to compose them freely and embed Common Lisp code within them.

```lisp
(hsx
  (div
    (p :id (+ 1 1))
    (ul
      (loop
        :for i :from 1 :to 3
        :collect (li (format nil "item~a" i))))
    (if t
        (p "true")
        (p "false"))))
```

This generates:

```html
<div>
  <p id="2"></p>
  <ul>
    <li>item1</li>
    <li>item2</li>
    <li>item3</li>
  </ul>
  <p>true</p>
</div>
```

To define a component, simply create a function that accepts keyword arguments, property lists, or both, and define the HSX element using the `defhsx` macro. The `children` property accepts the children of a component.

```lisp
(defhsx card #'%card)
(defun %card (&key title children)
  (hsx
    (div
      (h1 title)
      children)))
```

or

```lisp
(defhsx card #'%card)
(defun %card (&rest props)
  (hsx
    (div
      (h1 (getf props :title))
      (getf props :children))))
```

This can then be used as follows:

```lisp
(hsx
  (card :title "card1"
    (p "Lorem ipsum...")))
```

Which generates:

```html
<div>
  <h1>card1</h1>
  <p>Lorem ipsum...</p>
</div>
```

The previous definition can be simplified using the `defcomp` macro.

```lisp
(defcomp card (&key title children)
  (hsx
    (div
      (h1 title)
      children)))
```

## License

This project is licensed under the terms of the MIT license.

Copyright (c) 2024, skyizwhite.

Copyright (c) 2018, Bo Yao.
