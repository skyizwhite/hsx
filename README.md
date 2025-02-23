# HSX

HSX (Hypertext S-expression) is a simple and powerful HTML (Living Standard)
generation library for Common Lisp.

This project is a fork of [ailisp/flute](https://github.com/ailisp/flute/).

## Warning

This software is still in ALPHA quality. The APIs are likely to change.

Please check the [release notes](https://code.skyizwhite.dev/paku/hsx/releases)
for updates.

## Getting Started

### Basic Usage

Use the `hsx` macro to create HTML elements. Attributes are specified using a
property list after the element name, and child elements are nested directly
inside.

```lisp
(hsx
  (div :id "example" :class "container"
    (h1 "Welcome to HSX")
    (p "This is an example paragraph.")))
```

This generates:

```html
<div id="example" class="container">
  <h1>Welcome to HSX</h1>
  <p>This is an example paragraph.</p>
</div>
```

To convert an HSX object into an HTML string, use the `render-to-string`
function:

```lisp
(render-to-string
  (hsx ...))
```

### Embedding Content

HSX allows you to embed Common Lisp forms directly within your HTML structure.

When working with HSX elements inside embedded Lisp forms, you should use the
`hsx` macro again.

```lisp
(hsx
  (div
    (p :id (format nil "id-~a" (random 100)))
    (ul
      (loop
        :for i :from 1 :to 5 :collect
        (hsx (li (format nil "Item ~a" i)))))
    (if (> (random 10) 5)
        (hsx (p "Condition met!"))
        (hsx (p "Condition not met!")))))
```

This might generate:

```html
<div>
  <p id="id-42"></p>
  <ul>
    <li>Item 1</li>
    <li>Item 2</li>
    <li>Item 3</li>
    <li>Item 4</li>
    <li>Item 5</li>
  </ul>
  <p>Condition not met!</p>
</div>
```

### Using Fragments

To group multiple elements without adding an extra wrapper, use the fragment
`<>`.

```lisp
(hsx
  (<>
    (h1 "Grouped Elements")
    (p "First paragraph.")
    (p "Second paragraph.")))
```

This generates:

```html
<h1>Grouped Elements</h1>
<p>First paragraph.</p>
<p>Second paragraph.</p>
```

### Creating Components

You can define reusable components using the `defcomp` macro. Component names
must begin with a tilde (`~`). Properties should be declared using `&key`,
`&rest`, or both. The body must return an HSX element.

```lisp
(defcomp ~card (&key title children)
  (hsx
    (div :class "card"
      (h1 title)
      children)))
```

Alternatively, you can use a property list:

```lisp
(defcomp ~card (&rest props)
  (hsx
    (div :class "card"
      (h1 (getf props :title))
      (getf props :children))))
```

Usage example:

```lisp
(hsx
  (~card :title "Card Title"
    (p "This is a card component.")))
```

Generates:

```html
<div class="card">
  <h1>Card Title</h1>
  <p>This is a card component.</p>
</div>
```

## License

This project is licensed under the MIT License.

© 2024 Akira Tempaku

© 2018 Bo Yao
