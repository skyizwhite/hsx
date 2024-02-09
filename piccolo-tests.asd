(defsystem piccolo-tests
  :author "skyizwhite <paku@skyizwhite.dev>"
  :license "MIT"
  :class :package-inferred-system
  :pathname "tests"
  :depends-on ("rove"
               "piccolo-tests/escape")
  :perform (test-op (o c)
                    (symbol-call :rove '#:run c)))
