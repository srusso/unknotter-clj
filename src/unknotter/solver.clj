(ns unknotter.solver
  (:require [unknotter.reidemeister.twist :refer [get-twistable-edges get-untwistable-edges]]
            [unknotter.knot :refer [thistlethwaite-unknot]]
            [unknotter.reidemeister.poke :refer [get-pokable-edges get-unpokable-edge-pairs]]
            [unknotter.reidemeister.slide :refer [get-slidable-faces]]))

(defn- is-literally-unknot [knot] (<= (count knot) 2))

(defn- apply-random-move [knot beta]
  (let [numerical_weights [(Math/pow Math/E (- beta))
                           (Math/pow Math/E beta)
                           (Math/pow Math/E (* 2 (- beta)))
                           (Math/pow Math/E (* 2 beta))
                           1]
        options [(get-twistable-edges knot)
                 (get-untwistable-edges knot)
                 (get-pokable-edges knot)
                 (get-unpokable-edge-pairs knot)
                 (get-slidable-faces knot)]]

    ))

(defn is-unknot?
  "Returns true if the algorithm manages to find a sequence of reidemeister moves
   to turn the input knot into the unknot.
   Returns false otherwise.
   This means that a return value of true indicates 'this is certainly an unknot' while
   a return value of false means 'this is probably not an unknot'."
  [knot]
  (not
    (nil?
      (some is-literally-unknot
            ; TODO call (unknotter.knot/is-valid) to assert that every move creates a valid knot
            (take 2000 (iterate #(apply-random-move % 2) knot))))))

(defn -main []
  (println (is-unknot? thistlethwaite-unknot)))