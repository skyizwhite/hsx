(defsystem piccolo-test
  :author "paku <paku@skyizwhite.dev>"
  :license "MIT"
  :depends-on (:piccolo :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "piccolo")))))
