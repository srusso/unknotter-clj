(ns unknotter.core
  (:gen-class)
  (:require [unknotter.diagram :as diagram]))

(defn -main
  [& args]
  (println (diagram/load-knot-diagram :3_1)))