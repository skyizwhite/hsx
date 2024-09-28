(defpackage #:hsx-test/group
  (:use #:cl
        #:rove
        #:hsx/group))
(in-package #:hsx-test/group)

(defgroup fruit
  apple banana orange)

(deftest group-test
  (testing "defgroup"
    (ok (hash-table-p *fruit*))
    (ok (fruit-p :apple))
    (ng (fruit-p :tomato))))
