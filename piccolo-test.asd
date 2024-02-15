(defsystem piccolo-test
  :author "Bo Yao, skyizwhite"
  :maintainer "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :depends-on (:piccolo :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "piccolo")))))
