(defsystem "hsx"
  :version "1.1.0"
  :description "Component-oriented HTML DSL"
  :author "Akira Tempaku, Bo Yao"
  :maintainer "Akira Tempaku <paku@skyizwhite.dev>"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op hsx-test)))
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("hsx/main"))
