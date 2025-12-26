(ns unknotter.reidemeister.slide
  (:require [unknotter.vectors :refer [has overlap?]]
            [unknotter.knot-manipulation :refer [find-friend-crossing-index]]))

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
  (let [face-edges [edge1 edge2 edge3]]
    (map-indexed
      #(face-edges-in-crossing knot face-edges %1 %2)
      knot)))