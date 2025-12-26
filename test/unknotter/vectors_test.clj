(ns unknotter.vectors-test
  (:require [clojure.test :refer [deftest is testing]])
  (:require [unknotter.vectors :refer [index-of item-count-in has overlap? equal-as-set count-of]]))

(deftest index-of-test
  (testing
    (is (= 2 (index-of [1 2 3] 3)))))

(deftest count-of-test
  (testing
    (is (= 2 (count-of [1 2 3 4] even?)))
    (is (= 3 (count-of [[1 2] [] [] [] [1]] empty?)))))

(deftest item-count-in-test
  (testing
    (is (= 0 (item-count-in [1 2 3] 4)))
    (is (= 1 (item-count-in [1 2 3] 3)))
    (is (= 2 (item-count-in [1 2 3 3] 3)))))

(deftest has-test
  (testing
    (is (= true (has [1 2 3] 1)))
    (is (= false (has [1 2 3] 4)))))

(deftest overlap?-test
  (testing
    (is (= true (overlap? [1 2 3] [2 53])))
    (is (= false (overlap? [1 2 3] [4 55])))))

(deftest equal-as-set-test
  (testing
    (is (= true (equal-as-set [1 2 3] [1 2 3])))
    (is (= true (equal-as-set [1 2 3] [3 2 1])))
    (is (= true (equal-as-set [1 2 3] [3 1 2])))
    (is (= false (equal-as-set [1 2 3] [1 2 3 4])))
    (is (= false (equal-as-set [1 2 3] [1 2 4])))
    (is (= false (equal-as-set [1 2 3] [1 2])))))
