# HSX

⚠️ This project is a work in progress. [Roadmap](https://github.com/skyizwhite/hsx/issues/14)

HSX (Hypertext S-expression) is a straightforward HTML5 generation library for Common Lisp.

This project is a fork of [flute](https://github.com/ailisp/flute/), originally created by Bo Yao.

## Usage

With the `hsx` macro, you can construct HTML using S-expressions.

The property list (inline form is also available) following the element name is interpreted as attributes, while the remaining elements are interpreted as child elements.

When a property is given a boolean value:
- `t` results in the key being displayed without a value.
- `nil` results in the property not being displayed at all.
- Any other type of value results in the key-value pair being displayed.

```lisp
(hsx
  (div :id "greeting" :class "flex"
    (h1 "Hello World")
    (p
      "This is"
      (strong '(:class "red")
        "an example!"))))
```

This code generates the following HTML:

```html
<div id="greeting" class="flex">
  <h1>Hello World</h1>
  <p>
    This is
    <strong class="red">an example!</strong>
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

The fragment `<>` allows you to group multiple elements without introducing additional wrappers.

```lisp
(hsx
  (<>
    (h1 "Title")
    (p "This is a paragraph.")
    (p "This is another paragraph.")))
```

This generates:

```html
<h1>Title</h1>
<p>This is a paragraph.</p>
<p>This is another paragraph.</p>
```

You can create HSX components using the `defcomp` macro. Components are essentially functions that accept keyword arguments, a property list, or both.

The `children` property accepts the children of a component.

```lisp
(defcomp card (&key title children)
  (hsx
    (div
      (h1 title)
      children)))
```

or

```lisp
(defcomp card (&rest props)
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

To output HSX as an HTML string, use the `render` method. By default, pretty-printing is enabled, but you can disable it by enabling the `minify` option.

```lisp
(render (hsx ...))
; or
(render (hsx ...) :minify t)
```

## License

This project is licensed under the terms of the MIT license.

© 2024 skyizwhite

© 2018 Bo Yao
