(ns unknotter.reidemeister.poke
  (:require [unknotter.knot :refer [get-all-edges infinity-unknot-1 infinity-unknot-2 prev-edge]]
            [unknotter.knot-manipulation :refer [get-adjacent-faces is-closed is-open]]
            [unknotter.vectors :refer [has item-count-in]]))

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
                   (map #(item-count-in crossing %))
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
    There isn't really a nice way to make this function more readable; there are 8 possible situations, each
    producing two different new crossings. You simply need to do all eight by hand to check that these are correct.
    Referring to the diagram from the (prepare-poke) documentation, one of the eight possibilities is
    shown here: https://github.com/srusso/unknotter-clj/blob/main/resources/imgs/pokeknot.png"
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

(defn- get-pokable-edges-with [knot edge]
  (let [[face-ccw face-cw] (get-adjacent-faces knot edge)
        pokable-with (->> (concat face-ccw face-cw)
                          (map abs)
                          (filter #(not= edge %))
                          set)]
    (map (fn [e] [edge e]) pokable-with)))

(defn- is-unpokable [knot edge1 edge2]
  (or
    (and (is-open knot edge1) (is-closed knot edge2))
    (and (is-open knot edge2) (is-closed knot edge1))))

(defn get-pokable-edges [knot]
  (->> (get-all-edges knot)
       (map (fn [edge] (get-pokable-edges-with knot edge)))))

(defn- get-unpokable-edge-pairs- [knot]
  (let [unpokable-edge-pairs (->> (get-all-edges knot)
                                 (map #(get-adjacent-faces knot %))
                                 ; flatten the faces
                                 (apply concat)
                                 (filter (fn [face] (= 2 (count face))))
                                 (filter (fn [[edge1 edge2]] (is-unpokable knot (abs edge1) (abs edge2))))
                                 (map (fn [[edge1 edge2]] (sort [(abs edge1) (abs edge2)]))))]
    (reduce (fn [deduped-pairs [edge1 edge2]]
              (let [flattened-pairs (flatten deduped-pairs)]
                (if (or
                      (has flattened-pairs edge1)
                      (has flattened-pairs edge2))
                  deduped-pairs
                  (conj deduped-pairs [edge1 edge2]))))
            (set [])
            unpokable-edge-pairs)
    ))

(defn get-unpokable-edge-pairs [knot]
  (if (<= (count knot) 2)
    []
    (get-unpokable-edge-pairs- knot)))