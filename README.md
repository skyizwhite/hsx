# HSX (WIP)

HSX (hypertext s-expression) is an incredibly simple HTML5 generation library for Common Lisp.

This is a fork project of [flute](https://github.com/ailisp/flute/), originally created by Bo Yao.

# Usage

Using the `hsx` macro, you can implement HTML in S-expressions.

```
(hsx
  (div :id "greeting" :class "flex"
    (h1 "Hello World")
    (p
      "This is"
      (strong "example!"))))

          ↓ ↓ ↓

<div id="greeting" class="flex">
  <h1>Hello World</h1>
    <p>
      This is
      <strong>example!</strong>
    </p>
</div>
```

Elements in HSX are essentially functions, so you can freely compose them and embed CL code to them.

```
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

          ↓ ↓ ↓

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

To define a component, just define a function that takes keyword arguments or property list or both, and define hsx expression of it with `defhsx` macro.

```
(defhsx card #'%card)
(defun %card (&key title description)
  (hsx
    (div
      (h1 title)
      (p description))))

(hsx (card :title "card1" :description "brah brah brah..."))

          ↓ ↓ ↓

<div>
  <h1>card1</h1>
  <p>brah brah brah...</p>
</div>
```

The previous definition can be simplified by using the `defcomp` macro.

```
(defcomp card (&key title description)
  (hsx
    (div
      (h1 title)
      (p description))))
```

# License

This project is licensed under the terms of the MIT license.

Copyright (c) 2024, skyizwhite.

Copyright (c) 2018, Bo Yao.
