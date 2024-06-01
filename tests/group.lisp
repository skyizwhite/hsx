(defpackage #:hsx-test/group
  (:use #:cl
        #:fiveam
        #:hsx/group))
(in-package #:hsx-test/group)

(def-suite group-test)
(in-suite group-test)

(defgroup fruit
  apple banana orange)

(test defgroup
  (is (hash-table-p *fruit*))
  (is (fruit-p :apple))
  (is (not (fruit-p :tomato))))
