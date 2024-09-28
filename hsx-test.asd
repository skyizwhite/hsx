(defsystem "hsx-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "hsx-test/element"
               "hsx-test/escaper"
               "hsx-test/group"
               "hsx-test/hsx")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
