(ns unknotter.reidemeister.twist-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.knot :refer [knot=]]
            [unknotter.reidemeister.twist :refer [right-positive-twist]]
            [unknotter.resource-loader :refer [load-knot-diagram]]))

(def trefoil (load-knot-diagram :3_1))

(deftest test-poke
  (testing "Right positive twist"
    (is (knot= (right-positive-twist trefoil 1) [[2, 2, 3, 1], [4, 7, 5, 8], [6, 3, 7, 4], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 2) [[3, 3, 4, 2], [4, 7, 5, 8], [6, 1, 7, 2], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 3) [[2, 7, 3, 8], [4, 4, 5, 3], [6, 1, 7, 2], [8, 5, 1, 6]]))
    (is (knot= (right-positive-twist trefoil 4) [[2, 7, 3, 8], [5, 5, 6, 4], [6, 1, 7, 2], [8, 3, 1, 4]]))
    (is (knot= (right-positive-twist trefoil 5) [[2, 7, 3, 8], [4, 1, 5, 2], [6, 6, 7, 5], [8, 3, 1, 4]]))
    (is (knot= (right-positive-twist trefoil 6) [[2, 5, 3, 6], [4, 1, 5, 2], [7, 7, 8, 6], [8, 3, 1, 4]]))

    (is (knot= (right-positive-twist (load-knot-diagram :4_1) 1) [[2, 2, 3, 1], [3, 8, 4, 9], [5, 1, 6, 10], [7, 4, 8, 5], [9, 7, 10, 6]]))))
