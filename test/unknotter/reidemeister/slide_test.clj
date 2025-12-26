(ns unknotter.reidemeister.slide-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.resource-loader :refer [load-knot-diagram]]
            [unknotter.knot :refer [knot=]]
            [unknotter.reidemeister.slide :refer [slide]]))

(def trefoil (load-knot-diagram :3_1))

(deftest test-slide
  (testing "Fake trefoil slide"
    (is (knot=
          (slide [[4 1 5 2] [5 3 6 2] [6 3 1 4]] 2 4 6)
          [[3 2 4 3] [5 4 6 5] [6 2 1 1]])))
  (testing "Unknot 1 slide"
    (is (knot=
          (slide [[8 1 9 2] [2 9 3 10] [3 11 4 10] [7 5 8 4] [12 6 1 5] [6 12 7 11]] 5 7 12)
          [[8 1 9 2] [2 9 3 10] [3 11 4 10] [11 5 12 4] [6 6 7 5] [7 1 8 12]])))
  (testing "Unknot 2 slide"
    (is (knot=
          (slide [[2 11 3 12] [3 8 4 9] [4 10 5 9] [5 1 6 12] [6 1 7 2] [7 10 8 11]] 2 7 11)
          [[1 10 2 11] [3 8 4 9] [4 10 5 9] [5 1 6 12] [6 11 7 12] [7 2 8 3]]))))

(deftest test-slide-error-1
  (testing "Can only slide three edges along the same face."
    (is (thrown? IllegalArgumentException (slide trefoil 1 2 4)))))

(deftest test-slide-error-2
  (testing "Given edges do not follow the correct pattern for a slide."
    (is (thrown? IllegalArgumentException (slide trefoil 2 4 6)))))