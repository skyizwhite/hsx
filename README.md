# HSX

HSX (Hypertext S-expression) is a simple yet powerful HTML5 generation library for Common Lisp, forked from [flute](https://github.com/ailisp/flute/), originally created by Bo Yao.

## Introduction

HSX allows you to generate HTML using S-expressions, providing a more Lisp-friendly way to create web content. By using the `hsx` macro, you can define HTML elements and their attributes in a concise and readable manner.

## Getting Started

### Basic Usage

Use the `hsx` macro to create HTML elements. Attributes are specified using a property list following the element name, and child elements are nested directly within.

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

## Examples

### Dynamic Content

HSX allows embedding Common Lisp code directly within your HTML structure, making it easy to generate dynamic content.

```lisp
(hsx
  (div
    (p :id (format nil "id-~a" (random 100)))
    (ul
      (loop :for i :from 1 :to 5 :collect (li (format nil "Item ~a" i))))
    (if (> (random 10) 5)
        (p "Condition met!")
        (p "Condition not met!"))))
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

To group multiple elements without adding an extra wrapper, use the fragment `<>`.

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

## Creating Components

You can define reusable components with the `defcomp` macro. Components are functions that can take keyword arguments and properties.

```lisp
(defcomp card (&key title children)
  (hsx
    (div :class "card"
      (h1 title)
      children)))
```

Or using a property list:

```lisp
(defcomp card (&rest props)
  (hsx
    (div :class "card"
      (h1 (getf props :title))
      (getf props :children))))
```

Usage example:

```lisp
(hsx
  (card :title "Card Title"
    (p "This is a card component.")))
```

Generates:

```html
<div class="card">
  <h1>Card Title</h1>
  <p>This is a card component.</p>
</div>
```

## Rendering HTML

To render HSX to an HTML string, use the `render-to-string` function.

```lisp
(render-to-string
  (hsx
    (div :class "content"
      (h1 "Rendered to String")
      (p "This HTML is generated as a string."))))
```

## License

This project is licensed under the MIT License.

© 2024 skyizwhite

© 2018 Bo Yao

Feel free to contribute to the project and report any issues or feature requests on the [GitHub repository](https://github.com/skyizwhite/hsx).
