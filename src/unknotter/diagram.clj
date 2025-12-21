(ns unknotter.diagram
  "We use Planar Diagrams (PD) to represent knots. See: https://katlas.org/wiki/Planar_Diagrams")

(defn- load-resource-lines [resource-name]
  "Loads a resource as a vector of lines."
  (with-open [reader (clojure.java.io/reader (clojure.java.io/resource resource-name))]
    (reduce
      (fn [lines line]
        (conj lines line))
      []
      (line-seq reader))))

(defn- load-diagram-data []
  "Loads knotinfo.csv into a hash map.
  The map keys are the knot names from the file, but as clojure keywords, so :3_1 and so on."
  (->>
    (load-resource-lines "knotinfo.csv")
    (map (fn [line] (.split line ",")))
    (reduce (fn [knot-diagram-map [knot-name knot-data]]
              (assoc knot-diagram-map (keyword knot-name) knot-data))
            {})))

(def diagram-data (load-diagram-data))