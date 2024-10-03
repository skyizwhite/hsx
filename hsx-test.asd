(defsystem "hsx-test"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "hsx-test/utils"
               "hsx-test/element"
               "hsx-test/hsx")
  :perform (test-op (o c) (symbol-call :rove :run c :style :dot)))
