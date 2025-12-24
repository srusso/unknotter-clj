(ns unknotter.reidemeister)

(def infinity-unknot-1 [[1 2 2 1]])
(def infinity-unknot-2 [[1 1 2 2]])

(defn- next [knot edge])

(defn- prev [knot edge])

(defn- get_friend_index [knot crossing_index, edge_index])

(defn- get-forth-index [knot edge]
  (let [found-crossings
        (keep-indexed (fn [idx crossing]
                        (cond
                          (= edge (get crossing 0))
                          [idx 0]
                          (and (= edge (get crossing 1)) (= (next knot edge) (get crossing 3)))
                          [idx 1]
                          (and (= edge (get crossing 3)) (= (next knot edge) (get crossing 1)))
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