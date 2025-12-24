(ns unknotter.knot-manipulation-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.diagram :refer [load-knot-diagram]]
            [unknotter.knot-manipulation :refer [find-crossings-with-edge find-friend-crossing-index index-is-facing get-forth-index]]))

(deftest test-find-crossings-with-edge
  (testing "Get crossings with edge"
    (is (=
          [[0 [2 5 3 6]] [1 [4 1 5 2]]]
          (find-crossings-with-edge [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 2)))))

(deftest test-find-friend-index-illegal
  (testing "Find friend index in illegal knot"
    (is (thrown? IllegalArgumentException
                 ; too many 5s!
                 (find-friend-crossing-index [[2 5 3 6] [4 1 5 2] [5 3 1 4]] 0 1)))))

(deftest test-find-friend-index
  (testing "Find friend index"
    ; Explanation: at coordinates (0, 1) we find 5, which is also found at coordinates (1 2).
    (is (=
          [1 2]
          (find-friend-crossing-index [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 0 1)))
    ; Same test as above, but flipping the friend crossings.
    (is (=
          [0 1]
          (find-friend-crossing-index [[2 5 3 6] [4 1 5 2] [6 3 1 4]] 1 2)))
    ; Edge case: knot with a single crossing.
    (is (= [0 3] (find-friend-crossing-index [[1 2 2 1]] 0 0)))
    (is (= [0 0] (find-friend-crossing-index [[1 2 2 1]] 0 3)))))

(def trefoil [[2 5 3 6] [4 1 5 2] [6 3 1 4]])

(deftest test-index-is-facing
  (testing
    (is (= true (index-is-facing trefoil 0 0)))
    (is (= true (index-is-facing trefoil 0 1)))
    (is (= false (index-is-facing trefoil 0 2)))
    (is (= false (index-is-facing trefoil 0 3)))

    (is (= true (index-is-facing trefoil 1 0)))
    (is (= true (index-is-facing trefoil 1 1)))
    (is (= false (index-is-facing trefoil 1 2)))
    (is (= false (index-is-facing trefoil 1 3)))

    (is (= true (index-is-facing trefoil 2 0)))
    (is (= true (index-is-facing trefoil 2 1)))
    (is (= false (index-is-facing trefoil 2 2)))
    (is (= false (index-is-facing trefoil 2 3)))))

(def trefoil (load-knot-diagram :3_1))
(def knot72 (load-knot-diagram :7_2))

(deftest test-get-forth-index
  (testing
    (is (= (get-forth-index trefoil 1) [1, 1]))
    (is (= (get-forth-index trefoil 2) [0, 0]))
    (is (= (get-forth-index trefoil 3) [2, 1]))
    (is (= (get-forth-index trefoil 4) [1, 0]))
    (is (= (get-forth-index trefoil 5) [0, 1]))
    (is (= (get-forth-index trefoil 6) [2, 0]))

    (is (= (get-forth-index knot72 1) [3, 3]))
    (is (= (get-forth-index knot72 2) [0, 0]))
    (is (= (get-forth-index knot72 3) [6, 3]))
    (is (= (get-forth-index knot72 4) [1, 0]))
    (is (= (get-forth-index knot72 5) [5, 3]))
    (is (= (get-forth-index knot72 6) [2, 0]))
    (is (= (get-forth-index knot72 7) [4, 3]))
    (is (= (get-forth-index knot72 8) [3, 0]))
    (is (= (get-forth-index knot72 9) [0, 3]))
    (is (= (get-forth-index knot72 10) [4, 0]))
    (is (= (get-forth-index knot72 11) [2, 3]))
    (is (= (get-forth-index knot72 12) [5, 0]))
    (is (= (get-forth-index knot72 13) [1, 3]))))