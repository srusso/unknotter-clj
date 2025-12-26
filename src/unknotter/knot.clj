(ns unknotter.knot
  (:require [unknotter.knot-manipulation :refer [shifted]]
            [unknotter.vectors :refer [equal-as-set]]))

(def infinity-unknot-1 [[1 2 2 1]])
(def infinity-unknot-2 [[1 1 2 2]])

(defn knot= [knot1 knot2]
  (or
    (= knot1 knot2)
    (->> (range (* 2 (count knot1)))
         (map #(shifted knot1 %))
         (some #(equal-as-set % knot2)))))

(defn is-infinity-unknot [knot]
  (or (knot= knot infinity-unknot-1) (knot= knot infinity-unknot-2)))