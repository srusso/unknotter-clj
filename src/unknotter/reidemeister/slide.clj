(ns unknotter.reidemeister.slide
  (:require [unknotter.vectors :refer [has overlap?]]
            [unknotter.knot-manipulation :refer [find-friend-crossing-index]]))

(defn slide [knot edge1 edge2 edge3]
  (let [slide-edges [edge1 edge2 edge3]]
    (mapv identity
          (map-indexed
            (fn [crossing-index crossing]
              (map-indexed
                (fn [edge-index edge]
                  (cond
                    (and (overlap? crossing slide-edges) (has slide-edges (get crossing edge-index)))
                    (let [[friend-crossing-index friend-edge-index] (find-friend-crossing-index knot crossing-index edge-index)]
                      (get (get knot friend-crossing-index) (mod (+ friend-edge-index 2) 4)))
                    (overlap? crossing slide-edges)
                    (get crossing (mod (+ edge-index 2) 4))
                    :else edge
                    )
                  )
                crossing))
            knot))))