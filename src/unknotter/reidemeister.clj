(ns unknotter.reidemeister)

(def infinity-unknot-1 [[1 2 2 1]])
(def infinity-unknot-2 [[1 1 2 2]])

(defn- shift-modulo
  "Shift an edge by a given amount, wrapping around the number of edges."
  [knot edge amount]
  (+
    (mod (+ edge (- amount 1)) (* 2 (count knot)))
    1)
  )

(defn- next-edge [knot edge] (shift-modulo knot edge 1))

(defn- prev-edge [knot edge] (shift-modulo knot edge -1))

(defn- get-crossings-with-edge
  "Get a list of all crossings that are adjacent to a given edge."
  [knot edge]
  (filter
    (fn [crossing] (contains? crossing edge))
    knot))

(defn- get_friend_index
  "Given the index of an edge, get the index of its friend.

  Say we have a knot with two crossings: [[_  _  _  2] [_  _  2  _]].
  The first 2 is in the first crossing in the fourth position, so it has the index (1, 4).
  The second 2 is in the second crossing in the third position, so it has the index (2, 3).
  Thus, diagram._get_friend_index(1, 4) = (2, 3) and diagram._get_friend_index(2, 3) = (1, 4)."
  [knot crossing-index edge-index-within-crossing]
  (let [
        crossing (get knot crossing-index)
        edge (get crossing edge-index-within-crossing)
        crossings-with-edge (get-crossings-with-edge knot edge)
        ]
    (cond
      (= 1 (count crossings-with-edge))
      (= (first crossings-with-edge) (get knot crossing-index))
      :else
      )
    )
  )

(defn- index-is-facing
  "Returns true if the given edge is facing its crossing."
  [knot crossing-index edge-index]
  (let [crossing (get knot crossing-index)]
    (cond
      (= 0 edge-index) true
      (= 1 edge-index) (= (get crossing 3) (next (get crossing 1)))
      (= 3 edge-index) (= (get crossing 1) (next (get crossing 3)))
      :else (throw (IllegalArgumentException. (str "Invalid knot (could not determine facing index): " knot))))))

(defn- get-forth-index
  "Get the index of the given edge in the crossing it faces toward."
  [knot edge]
  (let [found-crossings
        (keep-indexed (fn [idx crossing]
                        (cond
                          (= edge (get crossing 0))
                          [idx 0]
                          (and (= edge (get crossing 1)) (= (next-edge knot edge) (get crossing 3)))
                          [idx 1]
                          (and (= edge (get crossing 3)) (= (next-edge knot edge) (get crossing 1)))
                          [idx 3]
                          :else nil
                          ))
                      knot)
        first-crossing (first found-crossings)]
    (when (nil? first-crossing)
      (throw (IllegalArgumentException. (str "Invalid knot (could not find forth index for edge " edge "): " knot))))
    first-crossing))

(defn- get-adjacent-faces
  "Given a knot and an edge, returns the two adjacent faces: counterclockwise first, clockwise second."
  [knot edge]
  [

   ])

(defn- prepare-poke
  "Readjust the edge values of the knot with the expectation of a poke between the two edges."
  [knot lower-edge higher-edge]
  )

(defn- poke-knot [knot under-edge over-edge]
  (let [lower-edge (min under-edge over-edge)
        higher-edge (max under-edge over-edge)
        [face-ccw face-cw] (get-adjacent-faces knot lower-edge)
        poked-knot (prepare-poke knot lower-edge higher-edge)
        [new-crossing-1 new-crossing-2]
        (cond
          (contains? face-cw (- higher-edge))
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3)] [(+ lower-edge 1), (+ higher-edge 4), (+ lower-edge 2), (+ higher-edge 3)]]
            [[(+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3), lower-edge] [(+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4), (+ lower-edge 2)]])
          (contains? face-ccw (- higher-edge))
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 2)] [(+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 4)]]
            [[(+ higher-edge 2), lower-edge, (+ higher-edge 3), (+ lower-edge 1)] [(+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 4), (+ lower-edge 1)]])
          (contains? face-cw higher-edge)
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 4), (+ lower-edge 1), (+ higher-edge 3)] [(+ lower-edge 1), (+ higher-edge 2), (+ lower-edge 2), (+ higher-edge 3)]]
            [[(+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2)] [(+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4), lower-edge]])
          (contains? face-ccw higher-edge)
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4)] [(+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 2)]]
            [[(+ higher-edge 2), (+ lower-edge 2), (+ higher-edge 3), (+ lower-edge 1)] [(+ higher-edge 3), lower-edge, (+ higher-edge 4), (+ lower-edge 1)]])
          :else (throw (IllegalArgumentException. "Can only poke edges along the same face.")))]
    (conj poked-knot new-crossing-1 new-crossing-2)))

(defn poke
  "Executes a poke on the specified knot edges."
  [knot under-edge over-edge]
  (when (= under-edge over-edge)
    (throw (IllegalArgumentException. "Cannot poke an edge underneath itself.")))
  (cond
    (= knot infinity-unknot-1) (if (= [under-edge over-edge] [1 2])
                                 [[1 4 2 5] [2 6 3 5] [3 6 4 1]]
                                 [[4 2 5 1] [5 2 6 3] [3 6 4 1]])
    (= knot infinity-unknot-2) (if (= [under-edge over-edge] [1 2])
                                 [[1 4 2 5] [2 6 3 5] [6 4 1 3]]
                                 [[4 2 5 1] [5 2 6 3] [6 4 1 3]])
    :else (poke-knot knot under-edge over-edge)))