(ns unknotter.reidemeister.twist
  (:require [unknotter.knot :refer [is-infinity-unknot get-all-edges next-edge next-edge-in-knot prev-edge shifted]]
            [unknotter.vectors :refer [item-count-in has]]))

(defn- lies-on-twist
  "Return true if the edge lies on a twist, i.e. the crossing has only two edges."
  [crossing edge]
  (= 2 (item-count-in crossing edge)))

(defn- prepare-twist [knot edge-to-twist]
  (mapv
    (fn [crossing]
      (if (lies-on-twist crossing edge-to-twist)
        (let [[edge1 edge2 edge3 edge4] crossing
              next (next-edge-in-knot knot)]
          (if (or
                (= edge2 (->> edge1 next))
                (= edge2 (->> edge1 next next)))
            [edge1, (+ edge2 2), edge3, (+ edge4 2)]
            [(+ edge1 2), edge2, (+ edge3 2), edge4]))
        (mapv
          (fn [edge]
            (if (or (< edge edge-to-twist)
                    (and (= edge edge-to-twist) (has crossing (prev-edge knot edge))))
              edge
              (+ edge 2)))
          crossing))
      )
    knot))

(defn- do-twist [knot edge create-crossing]
  (let [last-edge (* 2 (count knot))]
    (if (or (= edge 1) (= edge last-edge))
      (do-twist (shifted knot 1) (next-edge knot edge) create-crossing)
      (conj (prepare-twist knot edge) (create-crossing edge)))))

(defn left-positive-twist [knot edge-to-twist]
  (if
    (is-infinity-unknot knot)
    (if (= edge-to-twist 1)
      ; TODO exception if edge-to-twist is not either 1 or 2
      [[1, 2, 2, 3], [3, 1, 4, 4]]
      [[1, 4, 2, 1], [3, 2, 4, 3]])
    (do-twist knot
              edge-to-twist
              (fn [edge] [(+ edge 0), (+ edge 2), (+ edge 1), (+ edge 1)]))))

(defn left-negative-twist [knot edge-to-twist]
  (if
    (is-infinity-unknot knot)
    (if (= edge-to-twist 1)
      ; TODO exception if edge-to-twist is not either 1 or 2
      [[1, 2, 2, 3], [4, 3, 1, 4]]
      [[1, 4, 2, 1], [2, 4, 3, 3]])
    (do-twist knot
              edge-to-twist
              (fn [edge] [(+ edge 1), (+ edge 0), (+ edge 2), (+ edge 1)]))))

(defn right-positive-twist [knot edge-to-twist]
  (if
    (is-infinity-unknot knot)
    (if (= edge-to-twist 1)
      ; TODO exception if edge-to-twist is not either 1 or 2
      [[1, 2, 2, 3], [3, 4, 4, 1]]
      [[1, 4, 2, 1], [3, 3, 4, 2]])
    (do-twist knot
              edge-to-twist
              (fn [edge] [(+ edge 1), (+ edge 1), (+ edge 2), edge]))))

(defn right-negative-twist [knot edge-to-twist]
  (if
    (is-infinity-unknot knot)
    (if (= edge-to-twist 1)
      ; TODO exception if edge-to-twist is not either 1 or 2
      [[1, 2, 2, 3], [4, 4, 1, 3]]
      [[1, 4, 2, 1], [2, 3, 3, 4]])
    (do-twist knot
              edge-to-twist
              (fn [edge] [edge (+ edge 1) (+ edge 1) (+ edge 2)]))))

(def get-twistable-edges get-all-edges)

(defn- extract-edge-with-count-2 [crossing]
  (flatten (filter (fn [edge] (= 2 (item-count-in crossing edge))) crossing)))

(defn get-untwistable-edges [knot]
  (let [crossings-with-three-edges (filter #(= 3 (count (set %))) knot)]
    (extract-edge-with-count-2 crossings-with-three-edges)))

(defn get-untwistable-edges- [knot]
  (let [crossings-with-three-edges (filter #(= 3 (count (set %))) knot)]
    (map (fn [crossing]
           (apply max-key #(item-count-in crossing %) crossing))
         crossings-with-three-edges)))