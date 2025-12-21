(ns unknotter.core
  (:gen-class)
  (:require [unknotter.diagram :as diagram]))

(defn -main
  [& args]
  (println (diagram/diagram-data :3_1)))