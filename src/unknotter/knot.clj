(ns unknotter.knot
  (:require [unknotter.vectors :refer [equal-as-set]]
            [malli.core :as m]))

; schema for "vector of integer vectors, the inner vectors having a size of 4"
(def knot-diagram-schema [:vector [:vector {:min 4, :max 4} :int]])

(def infinity-unknot-1 [[1 2 2 1]])
(def infinity-unknot-2 [[1 1 2 2]])
(def thistlethwaite-unknot
  [[22, 2, 23, 1] [3 27 4 26] [5 21 6 20]
   [7 18 8 19] [9 25 10 24] [11 2 12 3]
   [4 13 5 14] [15 6 16 7] [30 17 1 18]
   [19 14 20 15] [28 22 29 21] [23 11 24 10]
   [25 9 26 8] [27 12 28 13] [16 29 17 30]])

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

(def knot-format-validator (m/validator knot-diagram-schema))

(defn validate-knot-format [knot]
  (let [valid? (knot-format-validator knot)]
    (when (not valid?)
      (throw (IllegalArgumentException. (str "Not a valid knot representation: " knot))))
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
  (mapv identity (range 1 (+ 1 (edge-count knot)))))

(defn first-or-last-edge? [knot edge]
  (or (= edge 1) (= edge (* 2 (count knot)))))