(ns unknotter.knot-manipulation
  (:require [unknotter.vectors :refer [index-of indexes-of has]]))

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
  (let [knot-edge-count (* 2 (count knot))]
    (+
      (mod (+ starting-edge (- steps 1)) knot-edge-count)
      1)))

(defn next-edge
  "Gets the edge after the given edge, following the knot's orientation.
  A new edge is born after each under-crossing and over-crossing."
  [knot edge] (walk-along-knot knot edge 1))

(defn prev-edge
  "Same as (next-edge) but walks backwards, i.e. returns the edge _before_ the given edge."
  [knot edge] (walk-along-knot knot edge -1))

(defn find-crossings-with-edge
  "Get a list of all crossings that are adjacent to a given edge.
   It returns either 1 or two crossings. Throws an exception if 0 or more than 2 crossings are found."
  [knot edge]
  (let [result (keep-indexed (fn [i crossing] (if (has crossing edge) [i crossing] nil)) knot)]
    (when (empty? result)
      (throw (IllegalArgumentException. (str "Could not find edge " edge " within knot: " knot))))
    (when (> (count result) 2)
      (throw (IllegalArgumentException. (str "Invalid knot. More than two crossings with edge " edge " found. Knot: " knot))))
    result))

(defn- get-edge [knot crossing-index edge-index]
  (get (get knot crossing-index) edge-index))

(defn find-friend-crossing-index
  "Given the index of an edge, get the index of its friend.

  Say we have a knot with two crossings: [[_  _  _  2] [_  _  2  _]].
  The first 2 is in the first crossing in the fourth position, so it has the index (1, 4).
  The second 2 is in the second crossing in the third position, so it has the index (2, 3).
  Thus, diagram._get_friend_index(1, 4) = (2, 3) and diagram._get_friend_index(2, 3) = (1, 4)."
  [knot crossing-index edge-index]
  (let [crossing (get knot crossing-index)
        edge (get crossing edge-index)
        [[idx1 crossing1] [idx2 crossing2]] (find-crossings-with-edge knot edge)]
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
  (let [[[idx1 crossing1] [idx2 crossing2]] (find-crossings-with-edge knot edge)
        edge-idx-in-crossing-1 (index-of crossing1 edge)]
    (if (index-is-facing knot idx1 edge-idx-in-crossing-1)
      [idx1 edge-idx-in-crossing-1]
      [idx2 (index-of crossing2 edge)])))

(defn- get-adjacent-face
  "Get one adjacent face. The direction is -1 for counterclockwise, 1 for clockwise."
  [knot edge direction]
  (let [
        ; Given a crossing index and an edge index, move the edge index according to the direction.
        move-edge-within-crossing (fn [[c e]] [c (mod (+ e direction) 4)])
        ; The next edge in the face, right after the initial edge 'edge'.
        indexes-of-next-edge (move-edge-within-crossing (get-forth-index knot edge))
        face-indexes (->> indexes-of-next-edge
                          (iterate (fn [edge-indexes]
                                     (->> (apply find-friend-crossing-index knot edge-indexes)
                                          move-edge-within-crossing))))
        sign #(if (apply index-is-facing knot %) -1 1)
        face-edges (->> face-indexes
                        (map #(* (sign %) (apply get-edge knot %))))]
    (->> face-edges
         (take-while #(not= % edge))
         ; Don't forget to prepend the initial edge itself!
         (concat [edge])
         (into []))))

(defn get-adjacent-faces
  "Given a knot and an edge, returns the two adjacent faces: counterclockwise first, clockwise second.
  The image below shows the clockwise adjacent face in green and counterclockwise in yello.
  Diagram: https://github.com/srusso/unknotter-clj/blob/main/resources/imgs/adjacentfaces.png
  TODO: Make nicer picture."
  [knot edge]
  [(get-adjacent-face knot edge -1) (get-adjacent-face knot edge 1)])

(defn shifted [knot shift]
  (mapv
    (fn [crossing]
      (mapv
        (fn [edge]
          (walk-along-knot knot edge shift))
        crossing))
    knot))