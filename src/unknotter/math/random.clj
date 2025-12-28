(ns unknotter.math.random)

(defn weighted-choice
  [items weights]
  (let [total (reduce + weights)
        r     (rand total)]
    (loop [[item & items] items
           [w & weights]  weights
           acc            w]
      (if (<= r acc)
        item
        (recur items weights (+ acc (first weights)))))))