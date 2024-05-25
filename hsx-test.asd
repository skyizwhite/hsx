(defsystem "hsx-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("fiveam"
               "hsx-test/element")
  :perform (test-op (op c) (symbol-call :fiveam :run-all-tests)))
