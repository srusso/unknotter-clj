(ns unknotter.vectors)

(defn index-of
  "Returns the index of the first occurrence of the element in the collection.
  If not found, returns nil."
  [collection element]
  (let [indexes (keep-indexed (fn [i el] (if (= element el) i nil)) collection)]
    (if (not-empty indexes) (first indexes) nil)))