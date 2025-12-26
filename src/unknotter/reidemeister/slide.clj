(ns unknotter.reidemeister.slide
  (:require [unknotter.vectors :refer [has overlap?]]
            [unknotter.knot-manipulation :refer [find-friend-crossing-index get-adjacent-faces]]))

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

    (when-not ()
      (throw (IllegalArgumentException. (str "Given edges do not follow the correct pattern for a slide."))))

    ;if not (all(edge in face_ccw or -edge in face_ccw for edge in edges) and len(face_ccw) == 3 or all(edge in face_cw or -edge in face_cw for edge in edges) and len(face_cw) == 3):
    ;    raise ReidemeisterError("can only slide three edges along the same face.")
    ;
    ;# Check if edges are layered properly
    ;if not _is_slidable(self, edge1, edge2, edge3):
    ;    raise ReidemeisterError("given edges do not follow the correct pattern for a slide.")


    (map-indexed
      #(face-edges-in-crossing knot face-edges %1 %2)
      knot)))