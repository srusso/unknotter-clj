(ns unknotter.reidemeister.slide
  (:require [unknotter.vectors :refer [has overlap?]]
            [unknotter.knot-manipulation :refer [find-friend-crossing-index get-adjacent-faces
                                                 is-open is-closed is-half-open]]))

(defn- is-slidable [knot [face-edge-1 face-edge-2 face-edge-3]]
  (or
    (and (is-open knot face-edge-1) (is-closed knot face-edge-2) (is-half-open knot face-edge-3))
    (and (is-open knot face-edge-1) (is-closed knot face-edge-3) (is-half-open knot face-edge-2))
    (and (is-open knot face-edge-2) (is-closed knot face-edge-1) (is-half-open knot face-edge-3))
    (and (is-open knot face-edge-2) (is-closed knot face-edge-3) (is-half-open knot face-edge-1))
    (and (is-open knot face-edge-3) (is-closed knot face-edge-1) (is-half-open knot face-edge-2))
    (and (is-open knot face-edge-3) (is-closed knot face-edge-2) (is-half-open knot face-edge-1))))

(defn- face-edges-in-crossing [knot face-edges crossing-index crossing]
  (let [updated-edge-value (fn [crossing edge-index] (get crossing (mod (+ edge-index 2) 4)))
        crossing-involves-face-edges (overlap? crossing face-edges)
        is-face-edge (fn [edge-index] (has face-edges (get crossing edge-index)))]
    (map-indexed
      (fn [edge-index edge]
        (cond
          (and crossing-involves-face-edges (is-face-edge edge-index))
          (let [[fci fei] (find-friend-crossing-index knot crossing-index edge-index)]
            (updated-edge-value (get knot fci) fei))
          crossing-involves-face-edges
          (updated-edge-value crossing edge-index)
          :else edge))
      crossing)))

(defn slide
  "Slide an edge over the face formed by the three given edges."
  [knot edge1 edge2 edge3]
  (let [face-edges [edge1 edge2 edge3]
        [face-ccw face-cw] (get-adjacent-faces knot edge1)]

    (when-not (or
                (and (every? #(or
                               (has face-ccw %)
                               (has face-ccw (- %))
                               ) face-edges)
                     (= 3 (count face-ccw)))
                (and (every? #(or
                                (has face-cw %)
                                (has face-cw (- %))
                                ) face-edges)
                     (= 3 (count face-cw))))
      (throw (IllegalArgumentException. (str "Can only slide three edges along the same face."))))

    (when-not (is-slidable knot face-edges)
      (throw (IllegalArgumentException. (str "Given edges do not follow the correct pattern for a slide."))))

    (map-indexed
      #(face-edges-in-crossing knot face-edges %1 %2)
      knot)))