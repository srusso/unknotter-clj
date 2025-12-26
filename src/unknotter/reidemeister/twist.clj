(ns unknotter.reidemeister.twist
  (:require [unknotter.knot :refer [is-infinity-unknot]]
            [unknotter.knot-manipulation :refer [next-edge prev-edge shifted]]
            [unknotter.vectors :refer [count-of has]]))

(defn- lies-on-twist
  "Return true if the edge lies on a twist, i.e. the crossing has only two edges."
  [crossing edge]
  (= 2 (count-of crossing edge)))

(defn- prepare-twist [knot edge-to-twist]
  (mapv
    (fn [crossing]
      (if (lies-on-twist crossing edge-to-twist)
        (if (or
              (= (next-edge knot (get crossing 0)) (get crossing 1))
              (= (next-edge knot (next-edge knot (get crossing 0))) (get crossing 1)))
          [(get crossing 0), (+ (get crossing 1) 2), (get crossing 2), (+ (get crossing 3) 2)]
          [(+ (get crossing 0) 2), (get crossing 1), (+ (get crossing 2) 2), (get crossing 3)])
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
      (let [prepared-knot (prepare-twist knot edge)]
        (conj prepared-knot (create-crossing edge))))))

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