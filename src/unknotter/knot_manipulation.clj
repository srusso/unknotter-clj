(ns unknotter.knot-manipulation
  (:require [unknotter.vectors :refer [index-of indexes-of]]))

(defn shift-modulo
  "Shift an edge by a given amount, wrapping around the number of edges."
  [knot edge amount]
  (+
    (mod (+ edge (- amount 1)) (* 2 (count knot)))
    1)
  )

(defn next-edge
  "Gets the edge after the given edge, following the knot's orientation.
  A new edge is born after each under-crossing and over-crossing."
  [knot edge] (shift-modulo knot edge 1))

(defn prev-edge [knot edge] (shift-modulo knot edge -1))

(defn find-crossings-with-edge
  "Get a list of all crossings that are adjacent to a given edge."
  [knot edge]
  (keep-indexed (fn [i crossing] (if (some #(= edge %) crossing) [i crossing] nil))
                knot))

(defn find-friend-crossing-index
  "Given the index of an edge, get the index of its friend.

  Say we have a knot with two crossings: [[_  _  _  2] [_  _  2  _]].
  The first 2 is in the first crossing in the fourth position, so it has the index (1, 4).
  The second 2 is in the second crossing in the third position, so it has the index (2, 3).
  Thus, diagram._get_friend_index(1, 4) = (2, 3) and diagram._get_friend_index(2, 3) = (1, 4)."
  [knot crossing-index edge-index]
  (let [crossing (get knot crossing-index)
        edge (get crossing edge-index)
        [[idx1 crossing1] [idx2 crossing2] & more] (find-crossings-with-edge knot edge)]

    (when (not-empty more)
      (throw (IllegalArgumentException. (str "Invalid knot. More than two crossings with edge " edge " found. Knot: " knot))))

    (cond
      ; We found only one crossing with the desired edge: it's its own friend then.
      (nil? crossing2) (let [[position1 position2] (indexes-of crossing edge)]
                         ; The edge will be in two places in this crossing. Return the other one.
                         [crossing-index (if (= position1 edge-index) position2 position1)])

      ; We found two crossings with the desired edge: return the other one.
      (= crossing1 crossing) [idx2 (index-of crossing2 edge)]
      :else [idx1 (index-of crossing1 edge)])))

(defn index-is-facing
  "Returns true if the given edge is facing its crossing.
   In PD notation:
    * the edge at index 0 is the incoming undercrossing (i.e. true return value)
    * the edge at index 2 is the outgoing undercrossing (i.e. false return value)
    * for edges 1 and 3, one of them will be facing the crossing, hence the implementation
      for one of those can simple be the negation of the other"
  [knot crossing-index edge-index]
  (let [crossing (get knot crossing-index)]
    (cond
      (= 0 edge-index) true
      (= 1 edge-index) (= (next-edge knot (get crossing 1)) (get crossing 3))
      (= 2 edge-index) false
      (= 3 edge-index) (not (index-is-facing knot crossing-index 1))
      :else (throw (IllegalArgumentException. (str "Invalid edge-index parameter: " edge-index ", knot: " knot))))))

(defn get-forth-index
  "Get the index of the given edge in the crossing it faces toward."
  [knot edge]
  (let [[[idx1 crossing1] [idx2 crossing2] & more] (find-crossings-with-edge knot edge)
        edge-idx-in-crossing-1 (index-of crossing1 edge)]
    (when (not-empty more)
      (throw (IllegalArgumentException. (str "Invalid knot. More than two crossings with edge " edge " found. Knot: " knot))))
    (if (index-is-facing knot idx1 edge-idx-in-crossing-1)
      [idx1 edge-idx-in-crossing-1]
      [idx2 (index-of crossing2 edge)])))

(defn get-adjacent-faces
  "Given a knot and an edge, returns the two adjacent faces: counterclockwise first, clockwise second."
  [knot edge]
  [

   ])