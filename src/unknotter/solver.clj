(ns unknotter.solver)

(defn- is-literally-unknot [knot] (<= (count knot) 2))

(defn- apply-random-move [knot]
  )

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
            (take 2000 (iterate apply-random-move knot))))))