(ns unknotter.knot-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.knot :refer [validate-knot-format]]))

(deftest test-knot-format-validation-valid-knot
  (testing
    (is (= [[1 2 3 4]] (validate-knot-format [[1 2 3 4]])))))

(deftest test-knot-format-validation-valid-knot
  (testing
    ; crossings should be vectors, not lists
    (is (thrown? IllegalArgumentException (validate-knot-format ['(1 2 3 4)])))
    ; knot should be a vector, not a list
    (is (thrown? IllegalArgumentException (validate-knot-format '([1 2 3 4]))))
    ; crossings should have 4 edges
    (is (thrown? IllegalArgumentException (validate-knot-format [[1 2 3]])))
    (is (thrown? IllegalArgumentException (validate-knot-format [[1 2 3 4 5]])))))