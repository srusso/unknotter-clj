(ns unknotter.knot-manipulation-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.resource-loader :refer [load-knot-diagram]]
            [unknotter.knot-manipulation :refer
             [find-crossings-with-edge find-friend-crossing-index index-is-facing get-forth-index get-adjacent-faces]]))

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

(deftest test-get-forth-index-twisted-edge
  (testing
    (is (= (get-forth-index [[24 4 25 3] [4 30 5 29] [5 23 6 22] [7 18 8 19] [9 27 10 26] [10 1 11 2] [2 11 3 12] [15 6 16 7] [32 17 1 18] [19 14 20 15] [30 24 31 23] [25 13 26 12] [27 9 28 8] [28 13 29 14] [16 31 17 32] [20 22 21 21]] 21)
           [15, 2]))))

(deftest test-get-adjacent-faces
  (testing
    (is (= (get-adjacent-faces trefoil 1) [[1, -4], [1, 5, 3]]))
    (is (= (get-adjacent-faces trefoil 2) [[2, 6, 4], [2, -5]]))
    (is (= (get-adjacent-faces trefoil 3) [[3, -6], [3, 1, 5]]))
    (is (= (get-adjacent-faces trefoil 4) [[4, 2, 6], [4, -1]]))
    (is (= (get-adjacent-faces trefoil 5) [[5, -2], [5, 3, 1]]))
    (is (= (get-adjacent-faces trefoil 6) [[6, 4, 2], [6, -3]]))

    (is (= (get-adjacent-faces knot72 1) [[1, 9, 3], [1, -8, 11, -6, 13, -4]]))
    (is (= (get-adjacent-faces knot72 2) [[2, -9], [2, 10, 8]]))
    (is (= (get-adjacent-faces knot72 3) [[3, 1, 9], [3, -14, 5, -12, 7, -10]]))
    (is (= (get-adjacent-faces knot72 4) [[4, -13, 6, -11, 8, -1], [4, 14]]))
    (is (= (get-adjacent-faces knot72 5) [[5, 13], [5, -12, 7, -10, 3, -14]]))
    (is (= (get-adjacent-faces knot72 6) [[6, -11, 8, -1, 4, -13], [6, 12]]))
    (is (= (get-adjacent-faces knot72 7) [[7, 11], [7, -10, 3, -14, 5, -12]]))
    (is (= (get-adjacent-faces knot72 8) [[8, -1, 4, -13, 6, -11], [8, 2, 10]]))
    (is (= (get-adjacent-faces knot72 9) [[9, 3, 1], [9, -2]]))
    (is (= (get-adjacent-faces knot72 10) [[10, -7, 12, -5, 14, -3], [10, 8, 2]]))
    (is (= (get-adjacent-faces knot72 11) [[11, 7], [11, -6, 13, -4, 1, -8]]))
    (is (= (get-adjacent-faces knot72 12) [[12, -5, 14, -3, 10, -7], [12, 6]]))
    (is (= (get-adjacent-faces knot72 13) [[13, 5], [13, -4, 1, -8, 11, -6]]))))

(deftest test-get-adjacent-faces-of-twisted-edge
  (testing
    (is (= (get-adjacent-faces [[24 4 25 3] [4 30 5 29] [5 23 6 22] [7 18 8 19] [9 27 10 26] [10 1 11 2] [2 11 3 12] [15 6 16 7] [32 17 1 18] [19 14 20 15] [30 24 31 23] [25 13 26 12] [27 9 28 8] [28 13 29 14] [16 31 17 32] [20 22 21 21]] 21)
           [[21], [21, -20, 15, -6, -22]]))))