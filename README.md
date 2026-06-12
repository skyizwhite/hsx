# HSX – HTML S-expression

HSX is a declarative, component-oriented way to write HTML in Common Lisp.
It lets you describe HTML structures and reusable components directly in Lisp, safely render them to HTML strings, and seamlessly integrate with your web applications.

→ [Example Project](https://github.com/skyizwhite/website)

---

## How It Works

HSX translates Lisp S-expressions into HTML by expanding them into calls to `create-element`.

Each tag or component inside an `(hsx ...)` form becomes:

```lisp
(create-element type props children)
```

For example:

```lisp
(hsx
  (article :class "container"
    (h1 "Title")
    (p "Paragraph")
    (~share-button :service :x)))
```

Expands into:

```lisp
(create-element :article
                (list :class "container")
                (list (create-element :h1 nil (list "Title"))
                      (create-element :p nil (list "Paragraph"))
                      (create-element #'~share-button
                                      (list :service :x)
                                      nil)))
```

---

## Quick Example

```lisp
(hsx
  (div :id "main" :class "container"
    (h1 "Hello, HSX!")
    (p "This is a simple paragraph.")))
```

↓

```html
<div id="main" class="container">
  <h1>Hello, HSX!</h1>
  <p>This is a simple paragraph.</p>
</div>
```

---

## Comparison with cl-who and spinneret

[cl-who](https://github.com/edicl/cl-who) and
[spinneret](https://github.com/ruricolist/spinneret) are both **compile-time
macros**: they expand an s-expression that is written literally at the call site
into Lisp code that writes HTML strings to a stream. Static parts are folded into
string literals at macro-expansion time, and only the dynamic parts remain as
runtime code.

HSX takes a different approach inspired by React/JSX. HSX builds a
**runtime tree of element objects**, which `render-to-string` then renders. This
makes elements first-class values you can return, store, and compose — at the
cost of doing the work at runtime instead of compile time.

| | cl-who | spinneret | HSX |
| --- | :---: | :---: | :---: |
| Rendering model | compile-time → stream code | compile-time → stream code | runtime element tree |
| Auto-escaping by default | ✗ (manual) | ✓ | ✓ |
| Components | ✗ | `deftag` | `defcomp` |
| Elements as values | ✗ | partial | ✓ |
| Fragments (no wrapper tag) | ✗ | ✗ | ✓ |
| Attribute validation | ✗ | ✓ | ✗ |

---

## Basic Usage

### Step 1: Create a Component

Components are defined using `defcomp`.
They are simple Lisp functions that return HSX elements.

Component names must start with `~` and props should be declared with `&key` and/or `&rest`.
The special `children` key automatically receives any nested elements.

```lisp
(defcomp ~button (&key href class children)
  (hsx
    (a :href href :class (clsx "btn" class)
      children)))
```

The special `rest` key gathers every prop that the component does not explicitly
declare into a plist, just like destructuring props in React. This is handy for
forwarding arbitrary attributes to the underlying element. Since `rest` is a
plist, merge it into the element's props with `append` (using the dynamic-props
form rather than inline keywords):

```lisp
(defcomp ~button (&key class children rest)
  (hsx
    (button (append (list :class (clsx "btn" class)) rest)
      children)))

;; (~button :type "submit" :data-id "42" "Save")
;; -> button gets :class "btn", :type "submit", :data-id "42"
```

Declared props (including `children`) are excluded from `rest`; any prop you do
not declare flows into it.

### Step 2: Combine Components

HSX allows composition of components just like JSX.

```lisp
(defcomp ~card (&key title children)
  (hsx
    (div :class "card"
      (h2 title)
      (div :class "content"
        children))))

(defparameter *view*
  (hsx
    (div :class "container"
      (~card :title "Hello"
        (~button :href "/start" :class "primary"
          "Get Started"))
      (~card :title "Docs"
        (p "Read the documentation to learn more.")))))
```

### Step 3: Render to HTML

Use `render-to-string` to produce a full HTML string.
Pass `:pretty t` for indented, human-readable output.

```lisp
(render-to-string *view* :pretty t)
```

Output:

```html
<div class="container">
  <div class="card">
    <h2>Hello</h2>
    <div class="content">
      <a href="/start" class="btn primary">Get Started</a>
    </div>
  </div>
  <div class="card">
    <h2>Docs</h2>
    <div class="content">
      <p>Read the documentation to learn more.</p>
    </div>
  </div>
</div>
```

---

## Fragments

### `<>` — Fragment

Combine multiple elements without creating an extra parent tag.

```lisp
(hsx
  (<>
    (li "One")
    (li "Two")))
```

↓

```html
<li>One</li>
<li>Two</li>
```

Fragments are useful when returning multiple sibling elements from a component.

### `raw!` — Raw Fragment

HSX automatically escapes unsafe characters in text and attribute values to prevent injection attacks.
If you need to insert raw, unescaped HTML, you can do so — but use it only with trusted content, as it disables automatic escaping and may expose security risks.

```lisp
(hsx
  (script (raw! "alert('unsafe if user-generated!')")))
```

---

## Expressions and Logic

You can embed any Lisp expression directly inside an HSX form.
Since HSX is just Lisp syntax, you can use if, when, loop, or any other macro to build dynamic content.

### Conditional Rendering

```lisp
(hsx
  (div
    (if (> (random 10) 5)
        (hsx (p "High!"))
        (hsx (p "Low!")))))
```

### Loop Rendering

```lisp
(hsx
  (ul
    (loop :for item :in items :collect
      (hsx (li item)))))
```

### Dynamic Props

HSX supports both inline plist props and dynamic plist props.

```lisp
(let ((props '(:class "btn" :href "/")))
  (hsx (a props "Dynamic Link")))
```

---

## Utilities

### `register-web-components`

Makes Web Components usable in HSX.

```lisp
(register-web-components
 custom1 custom2)

(hsx
  (custom1 :prop "val"
    (custom2)))
```

↓

```html
<custom1 prop="val">
  <custom2></custom2>
</custom1>
```

### `clear-web-components`

Clears all registered Web Components.

### `clsx`

Builds class strings conditionally.
Removes `nil` and joins the remaining strings with spaces.

```lisp
(clsx "btn" nil "primary")
;; => "btn primary"
```
---

## License

MIT License

© 2024 Akira Tempaku

© 2018 Bo Yao (original [flute](https://github.com/ailisp/flute) project)
 
