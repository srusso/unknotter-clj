(ns unknotter.diagram-test
  (:require [clojure.test :refer [deftest is testing]]
            [unknotter.diagram :refer [load-knot-diagram]]))

(deftest load-knot-diagram-test
  (testing "Knot Planar Diagram for 3_1 is loaded correctly, as a vector."
    (is (=
          (load-knot-diagram :3_1)
          [[2 5 3 6] [4 1 5 2] [6 3 1 4]])))
  (testing "Knot Planar Diagram for 7_4 is loaded correctly, as a vector."
    (is (=
          (load-knot-diagram :7_4)
          [[2 10 3 9] [4 12 5 11] [6 14 7 13] [8 4 9 3] [10 2 11 1] [12 8 13 7] [14 6 1 5]]))))
