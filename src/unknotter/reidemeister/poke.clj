(ns unknotter.reidemeister.poke
  (:require [unknotter.knot-manipulation :refer [get-adjacent-faces prev-edge]]
            [unknotter.vectors :refer [has count-of]]
            [unknotter.resource-loader :refer [infinity-unknot-1 infinity-unknot-2]]))

(defn prepare-poke
  "Readjust the edge values in crossings with the expectation of a poke between the two edges.
  Diagram: https://github.com/srusso/unknotter-clj/blob/main/resources/imgs/preparepoke.png
  TODO: Nicer diagram to explain this."
  [knot lower-edge higher-edge]
  (let [should-add-none (fn [crossing edge]
                          ; Leave edge alone IN THIS CROSSING if...
                          (or
                            ; it is before the lower edge
                            (< edge lower-edge)
                            (and
                              ; or, it is the lower edge and connects with the previous edge
                              (= edge lower-edge)
                              (has crossing (prev-edge knot edge)))))
        should-add-two (fn [crossing edge]
                         ; Add two if...
                         (or
                           ; it is the lower edge (by exclusion from the previous test, it connects with the next edge)
                           (= edge lower-edge)
                           ; or, it is between the lower and higher edges
                           (and (< lower-edge edge) (< edge higher-edge))
                           ; or, it is the higher edge, and it connects with the previous edge
                           (and (= edge higher-edge) (has crossing (prev-edge knot edge)))))]
    (mapv
      (fn [crossing]
        (when (->> [lower-edge higher-edge]
                   (map #(count-of crossing %))
                   (some #(= % 2)))
          (throw (UnsupportedOperationException. "Poke not implemented for edges that appear twice in the same crossing.")))
        (mapv
          (fn [edge]
            (cond
              (should-add-none crossing edge) edge
              (should-add-two crossing edge) (+ edge 2)
              ; Otherwise add four, if (by exclusion with previous tests):
              ;   * the edge comes after the higher edge
              ;   * or, it is the higher ege but connects with the next edge
              :else (+ edge 4)))
          crossing))
      knot)))

(defn- poke-knot
  "We poke a knot by:
    1. Modifying existing crossings via (prepare-poke) -> see documentation for that method, including diagram.
    2. Adding two new crossings. The new crossings depend on the clockwise and counterclockwise
       faces of the lower edge in the initial knot.
    Referring to the diagram from the (prepare-poke) documentation, the following exemplifies adding two new crossings
    in a simple example using the trefoil: https://github.com/srusso/unknotter-clj/blob/main/resources/imgs/pokeknot.png"
  [knot under-edge over-edge]
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

            ; This is the case (counterclockwise face contains -higher-edge, and the under-edge is the higher edge)
            ; shown in the diagram linked in the documentation of this method.
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