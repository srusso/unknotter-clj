(ns unknotter.vectors)

(defn indexes-of
  "Returns the indexes of all the occurrences of the element in the collection.
  If not found, returns an empty vector."
  [collection element]
  (mapv identity (keep-indexed (fn [i el] (if (= element el) i nil)) collection)))

(defn index-of
  "Returns the index of the first occurrence of the element in the collection.
  If not found, returns nil."
  [collection element]
  (first (indexes-of collection element)))