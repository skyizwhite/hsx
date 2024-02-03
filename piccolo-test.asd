(defsystem piccolo-test
  :author "Bo Yao <icerove@gmail.com>"
  :license "MIT"
  :depends-on (:piccolo :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "piccolo")))))
