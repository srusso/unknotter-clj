(ns unknotter.face
  (:require [unknotter.vectors :refer [overlap?]]))

(defn face=ignoring-direction
  "Returns true if the two faces are equal to each other.
   Ignores the direction of the edges."
  [face1 face2]
  (and (every? #(overlap? face1 [% (- %)]) face2)
       (= (count face1) (count face2))))