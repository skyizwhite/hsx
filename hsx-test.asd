(defsystem "hsx-test"
  :defsystem-depends-on ("fiveam-asdf")
  :class :package-inferred-fiveam-tester-system
  :pathname "tests"
  :depends-on ("hsx-test/element"
               "hsx-test/hsx"
               "hsx-test/escaper"
               "hsx-test/group")
  :test-names ((#:element-test . #:hsx-test/element)
               (#:hsx-test . #:hsx-test/hsx)
               (#:escaper-test . #:hsx-test/escaper)
               (#:group-test . #:hsx-test/group))
  :num-checks 44)
