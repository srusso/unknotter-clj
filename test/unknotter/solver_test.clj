(ns unknotter.solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.knot :refer [thistlethwaite-unknot]]
            [unknotter.reidemeister.poke :refer [unpoke]]
            [unknotter.reidemeister.slide :refer [slide]]
            [unknotter.reidemeister.twist :refer [left-positive-twist right-positive-twist untwist]]
            [unknotter.solver :refer [is-literally-unknot]]))


; Known moves to untangle the thistlethwaite unknot
(def moves
  [
   #(slide % 4 13 27)
   #(slide % 5 21 28)
   #(slide % 2 11 23)
   #(slide % 6 16 29)
   #(slide % 2 11 23)
   #(slide % 7 18 30)
   #(slide % 7 18 30)
   #(slide % 6 16 29)
   #(slide % 5 21 28)
   #(slide % 4 13 27)
   #(slide % 5 14 20)
   #(slide % 13 21 28)
   #(slide % 2 11 23)
   #(unpoke % 12 22)
   #(slide % 4 18 23)
   #(slide % 8 17 22)
   #(slide % 5 12 24)
   #(slide % 7 13 23)
   #(slide % 7 13 23)
   #(right-positive-twist % 6)
   #(untwist % 7)
   #(slide % 2 10 20)
   #(slide % 7 13 23)
   #(left-positive-twist % 18)
   #(untwist % 19)
   #(slide % 1 16 21)
   #(slide % 3 10 19)
   #(unpoke % 9 18)
   #(unpoke % 2 15)
   #(untwist % 12)
   #(unpoke % 11 16)
   #(untwist % 10)
   #(unpoke % 5 9)
   #(unpoke % 1 4)])

(defn- apply-test-moves
  [knot]
  (reduce (fn [acc-knot move] (move acc-knot)) knot moves))

(deftest test-unknot-detection
  (testing "Apply known untangle moves to thistlethwaite unknot"
    (is (= true (is-literally-unknot (apply-test-moves thistlethwaite-unknot))))))

