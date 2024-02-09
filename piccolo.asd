(defsystem "piccolo"
  :version "0.1.0"
  :description "A beautiful, easily composable HTML5 generation library"
  :author "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("piccolo/main")
  :in-order-to ((test-op (test-op "piccolo-tests"))))
