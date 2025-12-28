(ns unknotter.solver
  (:require [unknotter.knot :refer [thistlethwaite-unknot]]
            [unknotter.reidemeister.poke :refer [poke unpoke get-pokable-edge-pairs get-unpokable-edge-pairs]]
            [unknotter.reidemeister.slide :refer [slide get-slidable-faces]]
            [unknotter.math.random :refer [weighted-choice]]
            [unknotter.reidemeister.twist :refer [untwist right-negative-twist right-positive-twist left-negative-twist left-positive-twist get-twistable-edges get-untwistable-edges]])
  (:import (java.util Random)))

(defn is-literally-unknot [knot]
  (and
    (not-empty knot)
    (<= (count knot) 2)))

(def randomgen (Random.))
(def all-twists [right-negative-twist right-positive-twist left-negative-twist left-positive-twist])

(defn- apply-random-move [knot beta]
  (let [numerical-weights [(Math/pow Math/E (- beta))
                           (Math/pow Math/E beta)
                           (Math/pow Math/E (* 2 (- beta)))
                           (Math/pow Math/E (* 2 beta))
                           1]
        options [(get-twistable-edges knot)
                 (get-untwistable-edges knot)
                 (get-pokable-edge-pairs knot)
                 (get-unpokable-edge-pairs knot)
                 (get-slidable-faces knot)]
        weights (mapv (fn [[w o]] (if (= 0 (count o)) 0 w)) (zipmap numerical-weights options))
        chosen-move (weighted-choice [0 1 2 3 4] weights)]
    (cond
      (= 0 chosen-move)
      (let [twistable-edges (get options 0)
            edge (get twistable-edges (.nextInt randomgen 0 (count twistable-edges)))
            actual-twist (get all-twists (.nextInt randomgen 0 (count all-twists)))]
        (actual-twist knot edge))
      (= 1 chosen-move)
      (let [untwistable-edges (get options 1)
            edge (get untwistable-edges (.nextInt randomgen 0 (count untwistable-edges)))]
        (untwist knot edge))
      (= 2 chosen-move)
      (let [pokable-edge-pairs (get options 2)
            [edge1 edge2] (get pokable-edge-pairs (.nextInt randomgen 0 (count pokable-edge-pairs)))]
        (poke knot edge1 edge2))
      (= 3 chosen-move)
      (let [unpokable-edge-pairs (get options 3)
            [edge1 edge2] (get unpokable-edge-pairs (.nextInt randomgen 0 (count unpokable-edge-pairs)))]
        (unpoke knot edge1 edge2))
      (= 4 chosen-move)
      (let [slidable-faces (get options 4)
            [edge1 edge2 edge3] (get slidable-faces (.nextInt randomgen 0 (count slidable-faces)))]
        (slide knot edge1 edge2 edge3)))))

(defn apply-random-move-ignoring-unsupported-moves [knot beta]
  (try
    (apply-random-move knot beta)
    ; ignore and return the same knot unchanged in case the operation is not supported
    (catch UnsupportedOperationException e knot)))

(defn is-unknot?
  "Returns true if the algorithm manages to find a sequence of reidemeister moves
   to turn the input knot into the unknot.
   Returns false otherwise.
   So, a return value of true means 'this is certainly an unknot' while
   a return value of false means 'this is probably not an unknot'."
  [knot]
  (let [knots (take 2000 (iterate #(apply-random-move-ignoring-unsupported-moves % 2) knot))]
    (not
      (nil?
        (some is-literally-unknot knots)))))

(defn -main []
  (println (is-unknot? thistlethwaite-unknot)))