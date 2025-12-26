(ns unknotter.reidemeister.twist
  (:require [unknotter.knot-manipulation :refer [shifted next-edge]]))

(defn- prepare-twist [knot edge])

(defn right-positive-twist [knot edge]
  (let [last-edge (* 2 (count knot))]
    (if (or (= edge 1) (= edge last-edge))
      (right-positive-twist (shifted knot 1) (next-edge knot edge))
      (let [prepared-knot (prepare-twist knot edge)]
        (conj prepared-knot [(+ edge 1), (+ edge 1), (+ edge 2), edge])))))