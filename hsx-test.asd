(defsystem "hsx-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("fiveam"
               "hsx-test/element"
               "hsx-test/defhsx"
               "hsx-test/hsx"
               "hsx-test/renderer"
               "hsx-test/escaper")
  :perform (test-op (op c) (symbol-call :fiveam :run-all-tests)))
