(defsystem "hsx-test"
  :defsystem-depends-on ("fiveam-asdf")
  :class :package-inferred-fiveam-tester-system
  :pathname "tests"
  :depends-on ("fiveam"
               "hsx-test/element"
               "hsx-test/defhsx"
               "hsx-test/hsx"
               "hsx-test/escaper"
               "hsx-test/renderer")
  :test-names ((#:element-test . #:hsx-test/element)
               (#:defhsx-test . #:hsx-test/defhsx)
               (#:hsx-test . #:hsx-test/hsx)
               (#:escaper-test . #:hsx-test/escaper)
               (#:renderer-test . #:hsx-test/renderer))
  :num-checks 36)
