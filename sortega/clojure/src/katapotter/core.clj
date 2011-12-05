(ns katapotter.core)

(def unit-price 8)

(def discount {1 0.00
               2 0.05
               3 0.10
               4 0.20
               5 0.25
               })

(defn price-package [books]
  (let [bookcount (count books),
        factor (- 1 (discount bookcount))]
    (* bookcount unit-price factor)))

(defn price-packages [packages]
  (reduce + (map price-package packages)))

(defn all-selections
  "All possible pairs of [chosen rest]"
  [coll]
  (map #(vector 
          (nth coll %1) 
          (concat (take %1 coll) (nthrest coll (inc %1))))
       (range (count coll))))

(defn add-book
  "Adds a book to a package in the cheapest way"
  [packages book]
  (let [new-package (conj packages #{book}),
        elegible-selections (remove #(contains? (first %) book)
                                    (all-selections packages)),
        add-to-package (map
                         (fn [[p ps]] (conj ps (conj p book))) 
                         elegible-selections)
        all-options (conj add-to-package new-package)]
    (apply (partial min-key price-packages) all-options)))

(defn price 
  "Minimum possible price"
  [books]
  (let [packages (reduce add-book [] books)]
    (price-packages packages)))
