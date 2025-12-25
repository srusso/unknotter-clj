(ns unknotter.knot
  (:require [unknotter.knot-manipulation :refer [shifted]]
            [unknotter.vectors :refer [equal-as-set]]))

(defn knot= [knot1 knot2]
  (or
    (= knot1 knot2)
    (->> (range (* 2 (count knot1)))
         (map #(shifted knot1 %))
         (some #(equal-as-set % knot2)))))