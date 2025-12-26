(ns unknotter.reidemeister.slide
  (:require [unknotter.knot-manipulation :refer [find-friend-crossing-index get-adjacent-faces
                                                 is-closed is-half-open is-open]]
            [unknotter.vectors :refer [count-of has overlap?]]
            [unknotter.face :refer [face=ignoring-direction]]))

(defn- is-slidable
  "A face can be slided over if it has one open edge, one closed edge, and one half-open edge."
  [knot three-edged-face]
  (= 1
     (count-of three-edged-face #(is-open knot %))
     (count-of three-edged-face #(is-closed knot %))
     (count-of three-edged-face #(is-half-open knot %))))

(defn- slide-edges-in-crossing [knot three-edged-face crossing-index crossing]
  (let [updated-edge-value (fn [crossing edge-index] (get crossing (mod (+ edge-index 2) 4)))
        crossing-involves-face-edges (overlap? crossing three-edged-face)
        is-face-edge (fn [edge-index] (has three-edged-face (get crossing edge-index)))]
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
  (let [three-edged-face [edge1 edge2 edge3]
        [face-ccw face-cw] (get-adjacent-faces knot edge1)]

    (when-not (or (face=ignoring-direction face-ccw three-edged-face)
                  (face=ignoring-direction face-cw three-edged-face))
      (throw (IllegalArgumentException. (str "Can only slide three edges along the same face."))))

    (when-not (is-slidable knot three-edged-face)
      (throw (IllegalArgumentException. (str "Given edges do not follow the correct pattern for a slide."))))

    (map-indexed
      #(slide-edges-in-crossing knot three-edged-face %1 %2)
      knot)))

(defn get-slidable-faces [knot]
  )