(ns unknotter.knot
  (:require [unknotter.vectors :refer [equal-as-set]]))

(def infinity-unknot-1 [[1 2 2 1]])
(def infinity-unknot-2 [[1 1 2 2]])

(defn edge-count [knot]
  (* 2 (count knot)))

(defn- walk-along-knot
  "Walks along the knot by the specified amount of steps, starting from the specified edge.
   Return the edge encountered after the last step.
   Each step moves to the following edge (or previous if steps < 0).
   Imagine the trefoil, which has 6 edges (ignore crossings):
      -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 ->v
      |                               |
      ^--------<----------<-----------<
   Example: if you start from edge 2 and walk three steps, you end up at 5."
  [knot starting-edge steps]
  (+
    (mod (+ starting-edge (- steps 1)) (edge-count knot))
    1))

(defn next-edge
  "Gets the edge after the given edge, following the knot's orientation.
  A new edge is born after each under-crossing and over-crossing."
  [knot edge] (walk-along-knot knot edge 1))

(defn prev-edge
  "Same as (next-edge) but walks backwards, i.e. returns the edge _before_ the given edge."
  [knot edge] (walk-along-knot knot edge -1))

(defn next-edge-in-knot
  "Function that moves an edge to its next edge in the given knot."
  [knot]
  #(next-edge knot %))

(defn prev-edge-in-knot
  "Function that moves an edge to its previous edge in the given knot."
  [knot]
  #(prev-edge knot %))

(defn shifted [knot shift]
  (mapv
    (fn [crossing]
      (mapv
        (fn [edge]
          (walk-along-knot knot edge shift))
        crossing))
    knot))

(defn knot= [knot1 knot2]
  (or
    (= knot1 knot2)
    (->> (range (* 2 (count knot1)))
         (map #(shifted knot1 %))
         (some #(equal-as-set % knot2)))))

(defn is-infinity-unknot [knot]
  (or (knot= knot infinity-unknot-1) (knot= knot infinity-unknot-2)))

(defn is-valid [knot]
  (throw (UnsupportedOperationException. "Implement me.")))

(defn get-all-edges [knot]
  (range 1 (+ 1 (edge-count knot))))