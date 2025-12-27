(ns unknotter.solver
  (:require [unknotter.knot :refer [thistlethwaite-unknot]]
            [unknotter.reidemeister.poke :refer [poke unpoke get-pokable-edge-pairs get-unpokable-edge-pairs]]
            [unknotter.reidemeister.slide :refer [slide get-slidable-faces]]
            [unknotter.reidemeister.twist :refer [untwist right-negative-twist right-positive-twist left-negative-twist left-positive-twist get-twistable-edges get-untwistable-edges]])
  (:import (java.util Random)))

(defn- is-literally-unknot [knot] (and (not-empty knot) (<= (count knot) 2)))

(def randomgen (Random.))
(def all-twists [right-negative-twist right-positive-twist left-negative-twist left-positive-twist])

(defn- apply-random-move [knot beta]
  (let [numerical_weights [(Math/pow Math/E (- beta))
                           (Math/pow Math/E beta)
                           (Math/pow Math/E (* 2 (- beta)))
                           (Math/pow Math/E (* 2 beta))
                           1]
        options [(get-twistable-edges knot)
                 (get-untwistable-edges knot)
                 (get-pokable-edge-pairs knot)
                 (get-unpokable-edge-pairs knot)
                 (get-slidable-faces knot)]
        chosen-move (.nextInt randomgen 0 5)
        actual-moves (mapv (fn [i] (mod (+ i chosen-move) 5)) (range 0 5))
        actual-move (first (filter #(not (empty? (get options %))) actual-moves))]
    (cond
          (= 0 actual-move)
          (let [twistable-edges (get options 0)
                edge (get twistable-edges (.nextInt randomgen 0 (count twistable-edges)))
                actual-twist (get all-twists (.nextInt randomgen 0 (count all-twists)))]
            (actual-twist knot edge))
          (= 1 actual-move)
          (let [untwistable-edges (get options 1)
                edge (get untwistable-edges (.nextInt randomgen 0 (count untwistable-edges)))]
            (untwist knot edge))
          (= 2 actual-move)
          (let [pokable-edge-pairs (get options 2)
                [edge1 edge2] (get pokable-edge-pairs (.nextInt randomgen 0 (count pokable-edge-pairs)))]
            (poke knot edge1 edge2))
          (= 3 actual-move)
          (let [unpokable-edge-pairs (get options 3)
                [edge1 edge2] (get unpokable-edge-pairs (.nextInt randomgen 0 (count unpokable-edge-pairs)))]
            (unpoke knot edge1 edge2))
          (= 4 actual-move)
          (let [slidable-faces (get options 4)
                [edge1 edge2 edge3] (get slidable-faces (.nextInt randomgen 0 (count slidable-faces)))]
            (slide knot edge1 edge2 edge3)))))

(defn is-unknot?
  "Returns true if the algorithm manages to find a sequence of reidemeister moves
   to turn the input knot into the unknot.
   Returns false otherwise.
   So, a return value of true means 'this is certainly an unknot' while
   a return value of false means 'this is probably not an unknot'."
  [knot]
  (not
    (nil?
      (some is-literally-unknot
            ; TODO call (unknotter.knot/is-valid) to assert that every move creates a valid knot
            (mapv identity (take 2000 (iterate #(apply-random-move % 2) knot)))))))

(defn -main []
  (println (is-unknot? thistlethwaite-unknot)))