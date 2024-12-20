(defsystem "hsx"
  :version "0.4.0"
  :description "Simple and powerful HTML generation library."
  :author "skyizwhite, Bo Yao"
  :maintainer "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op hsx-test)))
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("hsx/main"))
