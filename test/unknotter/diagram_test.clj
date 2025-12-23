(ns unknotter.diagram-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.diagram :refer [load-knot-diagram]]))

(deftest a-test
  (testing "Knot Planar Diagram is loaded correctly, as a vector."
    (is (=
          (load-knot-diagram :3_1)
          [[2 5 3 6] [4 1 5 2] [6 3 1 4]]))))
