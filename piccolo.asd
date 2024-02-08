(defsystem "piccolo"
  :version "0.1.0"
  :description "A beautiful, easily composable HTML5 generation library"
  :author "paku <paku@skyizwhite.dev>, Bo Yao <icerove@gmail.com>"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op piccolo-test)))
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("piccolo/main"))
