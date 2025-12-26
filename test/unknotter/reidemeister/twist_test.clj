(ns unknotter.reidemeister.twist-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.knot :refer [infinity-unknot-1 infinity-unknot-2 knot=]]
            [unknotter.reidemeister.twist :refer [right-negative-twist right-positive-twist]]
            [unknotter.resource-loader :refer [load-knot-diagram]]))

(def trefoil (load-knot-diagram :3_1))

(deftest test-right-positive-twist
  (testing "Right positive twist"
    (is (knot= (right-positive-twist infinity-unknot-1 1) [[1, 2, 2, 3], [3, 4, 4, 1]]))
    (is (knot= (right-positive-twist infinity-unknot-1 2) [[1, 4, 2, 1], [3, 3, 4, 2]]))
    (is (knot= (right-positive-twist infinity-unknot-2 1) [[1, 2, 2, 3], [3, 4, 4, 1]]))
    (is (knot= (right-positive-twist infinity-unknot-2 2) [[1, 4, 2, 1], [3, 3, 4, 2]]))

    (is (knot= (right-positive-twist trefoil 1) [[2, 2, 3, 1], [4, 7, 5, 8], [6, 3, 7, 4], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 2) [[3, 3, 4, 2], [4, 7, 5, 8], [6, 1, 7, 2], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 3) [[2, 7, 3, 8], [4, 4, 5, 3], [6, 1, 7, 2], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 4) [[2, 7, 3, 8], [5, 5, 6, 4], [6, 1, 7, 2], [8, 3, 1, 4]]))
    (is (knot= (right-positive-twist trefoil 5) [[2, 7, 3, 8], [4, 1, 5, 2], [6, 6, 7, 5], [8, 3, 1, 4]]))
    (is (knot= (right-positive-twist trefoil 6) [[2, 5, 3, 6], [4, 1, 5, 2], [7, 7, 8, 6], [8, 3, 1, 4]]))

    (is (knot= (right-positive-twist (load-knot-diagram :4_1) 1) [[2, 2, 3, 1], [3, 8, 4, 9], [5, 1, 6, 10], [7, 4, 8, 5], [9, 7, 10, 6]]))))

(deftest test-right-negative-twist
  (testing "Right negative twist"
    (is (knot= (right-negative-twist infinity-unknot-1 1) [[1, 2, 2, 3], [4, 4, 1, 3]]))
    (is (knot= (right-negative-twist infinity-unknot-1 2) [[1, 4, 2, 1], [2, 3, 3, 4]]))
    (is (knot= (right-negative-twist infinity-unknot-2 1) [[1, 2, 2, 3], [4, 4, 1, 3]]))
    (is (knot= (right-negative-twist infinity-unknot-2 2) [[1, 4, 2, 1], [2, 3, 3, 4]]))

    (is (knot= (right-negative-twist trefoil 1) [[5, 8, 6, 1], [7, 4, 8, 5], [1, 6, 2, 7], [2, 3, 3, 4]]))
    (is (knot= (right-negative-twist trefoil 2) [[4, 7, 5, 8], [6, 1, 7, 2], [8, 5, 1, 6], [2, 3, 3, 4]]))
    (is (knot= (right-negative-twist trefoil 3) [[2, 7, 3, 8], [6, 1, 7, 2], [8, 5, 1, 6], [3, 4, 4, 5]]))
    (is (knot= (right-negative-twist trefoil 4) [[2, 7, 3, 8], [6, 1, 7, 2], [8, 3, 1, 4], [4, 5, 5, 6]]))
    (is (knot= (right-negative-twist trefoil 5) [[2, 7, 3, 8], [4, 1, 5, 2], [8, 3, 1, 4], [5, 6, 6, 7]]))
    (is (knot= (right-negative-twist trefoil 6) [[6, 1, 7, 2], [8, 5, 1, 6], [4, 7, 5, 8], [2, 3, 3, 4]]))

    (is (knot= (right-negative-twist (load-knot-diagram :4_1) 1) [[4, 9, 5, 10], [6, 2, 7, 1], [8, 5, 9, 6], [10, 8, 1, 7], [2, 3, 3, 4]]))))
