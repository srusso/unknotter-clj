(ns unknotter.reidemeister.poke-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.knot :refer [infinity-unknot-1 infinity-unknot-2 knot= thistlethwaite-unknot]]
            [unknotter.reidemeister.poke :refer [poke prepare-poke unpoke get-pokable-edge-pairs]]
            [unknotter.vectors :refer [equal-as-set]]
            [unknotter.resource-loader :refer [load-knot-diagram]]))

(def trefoil (load-knot-diagram :3_1))
(def expected-poked-trefoil [[2 7 3 8] [3 9 4 8] [4 9 5 10] [6 1 7 2] [10 5 1 6]])

(deftest test-prepare-poke
  (testing
    (is (= [[4 9 5 10] [8 3 9 4] [10 5 1 6]] (prepare-poke trefoil 1 4))))
  (testing
    (is (= [[2 9 3 10] [6 1 9 2] [10 3 1 4]] (prepare-poke trefoil 4 1))))
  (testing
    (is (= [[2 14 3 13] [6 18 9 17] [10 16 11 15] [12 2 13 1] [14 12 15 11] [16 10 17 9] [18 4 1 3]] (prepare-poke (load-knot-diagram :7_2) 4 1)))))

(deftest test-poke
  (testing "Poke trefoil into star, under-edge: 1, over-edge: 4"
    (is (knot= (poke trefoil 1 4) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 4, over-edge: 1"
    (is (knot= (poke trefoil 4 1) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 2, over-edge: 5"
    (is (knot= (poke trefoil 2 5) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 5, over-edge: 2"
    (is (knot= (poke trefoil 5 2) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 3, over-edge: 6"
    (is (knot= (poke trefoil 3 6) expected-poked-trefoil)))
  (testing "Poke trefoil into star, under-edge: 6, over-edge: 3"
    (is (knot= (poke trefoil 6 3) expected-poked-trefoil))))

(deftest test-poke-infinity-unknots
  (testing "Poke infinity unknot"
    (is (knot=
          (poke infinity-unknot-1 1 2)
          [[1 4 2 5] [2 6 3 5] [3 6 4 1]])))
  (testing "Poke infinity unknot 2"
    (is (knot=
          (poke infinity-unknot-2 2 1)
          [[4 2 5 1] [5 2 6 3] [6 4 1 3]]))))

(deftest test-illegal-poke
  (testing
    (is (thrown? IllegalArgumentException (poke trefoil 3 3)))))

(deftest unpoke-test
  (testing
    (is (knot= trefoil (unpoke (poke trefoil 3 6) 4 9)))))

(def pokable-thistlethwaite [[1, 23],
                            [1, 10],
                            [1, 25],
                            [1, 8],
                            [1, 18],
                            [1, 22],
                            [1, 29],
                            [1, 17],
                            [2, 11],
                            [2, 23],
                            [2, 12],
                            [2, 28],
                            [2, 22],
                            [3, 26],
                            [3, 9],
                            [3, 24],
                            [3, 11],
                            [3, 27],
                            [3, 12],
                            [4, 14],
                            [4, 19],
                            [4, 8],
                            [4, 26],
                            [4, 13],
                            [4, 27],
                            [5, 20],
                            [5, 14],
                            [5, 21],
                            [5, 28],
                            [5, 13],
                            [6, 15],
                            [6, 20],
                            [6, 16],
                            [6, 29],
                            [6, 21],
                            [7, 19],
                            [7, 15],
                            [7, 18],
                            [7, 30],
                            [7, 16],
                            [8, 26],
                            [8, 4],
                            [8, 14],
                            [8, 19],
                            [8, 25],
                            [8, 10],
                            [8, 23],
                            [8, 1],
                            [8, 18],
                            [9, 24],
                            [9, 11],
                            [9, 3],
                            [9, 26],
                            [9, 25],
                            [10, 24],
                            [10, 23],
                            [10, 1],
                            [10, 18],
                            [10, 8],
                            [10, 25],
                            [11, 3],
                            [11, 26],
                            [11, 9],
                            [11, 24],
                            [11, 2],
                            [11, 23],
                            [12, 27],
                            [12, 3],
                            [12, 28],
                            [12, 22],
                            [12, 2],
                            [13, 4],
                            [13, 27],
                            [13, 5],
                            [13, 21],
                            [13, 28],
                            [14, 19],
                            [14, 8],
                            [14, 26],
                            [14, 4],
                            [14, 20],
                            [14, 5],
                            [15, 7],
                            [15, 19],
                            [15, 6],
                            [15, 20],
                            [16, 30],
                            [16, 18],
                            [16, 7],
                            [16, 29],
                            [16, 21],
                            [16, 6],
                            [17, 30],
                            [17, 1],
                            [17, 22],
                            [17, 29],
                            [18, 7],
                            [18, 16],
                            [18, 30],
                            [18, 8],
                            [18, 25],
                            [18, 10],
                            [18, 23],
                            [18, 1],
                            [19, 15],
                            [19, 7],
                            [19, 14],
                            [19, 4],
                            [19, 26],
                            [19, 8],
                            [20, 6],
                            [20, 15],
                            [20, 5],
                            [20, 14],
                            [21, 29],
                            [21, 16],
                            [21, 6],
                            [21, 28],
                            [21, 13],
                            [21, 5],
                            [22, 1],
                            [22, 17],
                            [22, 29],
                            [22, 2],
                            [22, 12],
                            [22, 28],
                            [23, 10],
                            [23, 25],
                            [23, 8],
                            [23, 18],
                            [23, 1],
                            [23, 11],
                            [23, 2],
                            [24, 10],
                            [24, 9],
                            [24, 26],
                            [24, 3],
                            [24, 11],
                            [25, 8],
                            [25, 18],
                            [25, 1],
                            [25, 23],
                            [25, 10],
                            [25, 9],
                            [26, 4],
                            [26, 14],
                            [26, 19],
                            [26, 8],
                            [26, 3],
                            [26, 11],
                            [26, 24],
                            [26, 9],
                            [27, 13],
                            [27, 4],
                            [27, 12],
                            [27, 3],
                            [28, 21],
                            [28, 5],
                            [28, 13],
                            [28, 22],
                            [28, 2],
                            [28, 12],
                            [29, 16],
                            [29, 6],
                            [29, 21],
                            [29, 17],
                            [29, 1],
                            [29, 22],
                            [30, 18],
                            [30, 7],
                            [30, 16],
                            [30, 17]])

(deftest get-pokable-test
  (testing
    (is (equal-as-set pokable-thistlethwaite (get-pokable-edge-pairs thistlethwaite-unknot)))))