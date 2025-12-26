(ns unknotter.vectors)

(defn equal-as-set [collection-1 collection-2]
  (= (set collection-1) (set collection-2)))

(defn has [collection item]
  (not (nil? (some #(= item %) collection))))

(defn indexes-of
  "Returns the indexes of all the occurrences of the element in the collection.
  If not found, returns an empty vector."
  [collection element]
  (mapv identity (keep-indexed (fn [i el] (if (= element el) i nil)) collection)))

(defn item-count-in
  "Count occurrences of the item in the collection."
  [collection item]
  (->> collection
       (filter #(= % item))
       (count)))

(defn overlap? [collection1 collection2]
  (let [collection2-set (set collection2)]
    (not (nil? (->> collection1
                    (filter #(.contains collection2-set %))
                    (first))))))

(defn index-of
  "Returns the index of the first occurrence of the element in the collection.
  If not found, returns nil."
  [collection element]
  (first (indexes-of collection element)))