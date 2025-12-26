(ns unknotter.face
  (:require [unknotter.vectors :refer [overlap? equal-as-set]]))

(defn- ignoring-edge-direction [face]
  (map abs face))

(defn face=ignoring-direction
  "Returns true if the two faces are equal to each other.
   Ignores the edge order and their direction."
  [face1 face2]
  (and (equal-as-set (ignoring-edge-direction face1) (ignoring-edge-direction face2))
       (= (count face1) (count face2))))