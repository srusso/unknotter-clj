(ns unknotter.reidemeister
  (:require [unknotter.knot-manipulation :refer [get-adjacent-faces prev-edge]]
            [unknotter.vectors :refer [has]]
            [unknotter.diagram :refer [infinity-unknot-1 infinity-unknot-2]]))

(defn prepare-poke
  "Readjust the edge values of the knot with the expectation of a poke between the two edges.
    If a given edge comes before the lower edge, leave it alone.
    If a given edge is between the lower and higher edges, add two.
    If a given edge comes after the higher edge, add four.
    If it is the lower edge, leave it alone if it connects with the
    previous edge or add two if it connects with the next edge.
    If it is the higher edge, add two if it connects with the previous
    edge or add four if it connects with the next edge."
  [knot lower-edge higher-edge]
  (let [should-add-none (fn [crossing edge]
                          (or
                            (< edge lower-edge)
                            (and
                              (= edge lower-edge)
                              (has crossing (prev-edge knot edge)))))
        should-add-two (fn [crossing edge]
                         (or
                           (= edge lower-edge)
                           (and (< lower-edge edge) (< edge higher-edge))
                           (and (= edge higher-edge) (has crossing (prev-edge knot edge)))))]
    (mapv
      (fn [crossing]
        (mapv
          (fn [edge]
            (cond
              (should-add-none crossing edge) edge
              (should-add-two crossing edge) (+ edge 2)
              :else (+ edge 4)))
          crossing))
      knot)))

(defn- poke-knot [knot under-edge over-edge]
  (let [lower-edge (min under-edge over-edge)
        higher-edge (max under-edge over-edge)
        [face-ccw face-cw] (get-adjacent-faces knot lower-edge)
        poked-knot (prepare-poke knot lower-edge higher-edge)
        new-crossings
        (cond
          (has face-cw (- higher-edge))
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3)]
             [(+ lower-edge 1), (+ higher-edge 4), (+ lower-edge 2), (+ higher-edge 3)]]
            [[(+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3), lower-edge]
             [(+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4), (+ lower-edge 2)]])
          (has face-ccw (- higher-edge))
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 2)]
             [(+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 4)]]
            [[(+ higher-edge 2), lower-edge, (+ higher-edge 3), (+ lower-edge 1)]
             [(+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 4), (+ lower-edge 1)]])
          (has face-cw higher-edge)
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 4), (+ lower-edge 1), (+ higher-edge 3)]
             [(+ lower-edge 1), (+ higher-edge 2), (+ lower-edge 2), (+ higher-edge 3)]]
            [[(+ higher-edge 2), (+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2)]
             [(+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4), lower-edge]])
          (has face-ccw higher-edge)
          (if (= under-edge lower-edge)
            [[lower-edge, (+ higher-edge 3), (+ lower-edge 1), (+ higher-edge 4)]
             [(+ lower-edge 1), (+ higher-edge 3), (+ lower-edge 2), (+ higher-edge 2)]]
            [[(+ higher-edge 2), (+ lower-edge 2), (+ higher-edge 3), (+ lower-edge 1)]
             [(+ higher-edge 3), lower-edge, (+ higher-edge 4), (+ lower-edge 1)]])
          :else (throw (IllegalArgumentException. "Can only poke edges along the same face.")))]
    (concat poked-knot new-crossings)))

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