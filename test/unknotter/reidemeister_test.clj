(ns unknotter.reidemeister-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.diagram :refer [load-knot-diagram]]
            [unknotter.reidemeister :refer [poke get-friend-index get-crossings-with-edge]]))

(def trefoil (load-knot-diagram :3_1))
(def expected-poked-trefoil [[2 7 3 8] [3 9 4 8] [4 9 5 10] [6 1 7 2] [10 5 1 6]])

(deftest test-get-crossings-with-edge
  (testing "Get crossings with edge"
    (is (=
          [[0 [2 5 3 6]] [1 [4 1 5 2]]]
          (get-crossings-with-edge [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 2)))))

(deftest test-get-friend-index
  (testing "Find friend index"
    ; Explanation: at coordinates (0, 1) we find 5, which is also found at coordinates (1 2).
    (is (=
          [1 2]
          (get-friend-index [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 0 1)))
    ; Same test as above, but flipping the friend crossings.
    (is (=
          [0 1]
          (get-friend-index [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 1 2)))
    ; Edge case: knot with a single crossing.
    (is (= [0 3] (get-friend-index [[1 2 2 1]] 0 0)))
    (is (= [0 0] (get-friend-index [[1 2 2 1]] 0 3)))))

(deftest test-poke
  (testing "Poke trefoil into star, under-edge: 1, over-edge: 4"
    (is (= (poke trefoil 1 4) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 4, over-edge: 1"
    (is (= (poke trefoil 4 1) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 2, over-edge: 5"
    (is (= (poke trefoil 2 5) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 5, over-edge: 2"
    (is (= (poke trefoil 5 2) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 3, over-edge: 6"
    (is (= (poke trefoil 3 6) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 6, over-edge: 3"
    (is (= (poke trefoil 6 3) expected-poked-trefoil))))

(deftest test-poke-infinity-unknots
  (testing "Poke infinity unknot"
    (is (=
          (poke [[1 2 2 1]] 1 2)
          [[1 4 2 5] [2 6 3 5] [3 6 4 1]])))
  (testing "Poke infinity unknot 2"
    (is (=
          (poke [[1 1 2 2]] 2 1)
          [[4 2 5 1] [5 2 6 3] [6 4 1 3]]))))

(deftest test-illegal-poke
  (testing
    (is (thrown? IllegalArgumentException (poke trefoil 3 3)))))