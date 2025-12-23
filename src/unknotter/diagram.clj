(ns unknotter.diagram
  "We use Planar Diagrams (PD) to represent knots. See: https://katlas.org/wiki/Planar_Diagrams"
  (:require [clojure.java.io]))

(defn- load-resource-lines
  "Loads a resource as a vector of lines."
  [resource-name]
  (with-open [reader (clojure.java.io/reader (clojure.java.io/resource resource-name))]
    ; Use mapv instead of map because mapv is eager. Instead, map is lazy, and it would execute after the reader is closed.
    (mapv identity (line-seq reader))))

(defn- load-diagram-data
  "Loads knotinfo.csv into a hash map.
  The map keys are the knot names from the file, but as clojure keywords, so :3_1 and so on."
  []
  (->>
    (load-resource-lines "knotinfo.csv")
    (map (fn [line] (.split line ",")))
    (reduce (fn [knot-diagram-map [knot-name knot-data]]
              (assoc knot-diagram-map (keyword knot-name) knot-data))
            {})))

(def diagram-data (load-diagram-data))

(defn load-knot-diagram
  "Loads the diagram for a know. The knot-name is expected to be a keyword, such as :3_1 or :5_2."
  [knot-name]
  (let [knot-data (diagram-data knot-name)]
    ; knot-data looks like [[2;5;3;6];[4;1;5;2];[6;3;1;4]] etc. etc.
    (-> knot-data
        (.replace "];[", "] [")
        (.replace ";" " ")
        (read-string))))