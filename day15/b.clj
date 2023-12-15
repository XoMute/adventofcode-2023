(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn hash [^String str]
  (reduce
   (fn [value char]
     (mod (* (+ value (int char)) 17) 256))
   0
   str))

(defn update-boxes [boxes ^String op]
  (cond
    (.contains op "=")
    (let [[label focal] (str/split op #"=")
          focal (bigint focal)
          idx (hash label)
          lenses (boxes idx)
          lens (first (filter #(= (first %) label) lenses))
          ]
      (if (seq lenses)
        (if lens
          ;; found the lens, just update it's value
          (assoc-in boxes [idx (.indexOf lenses lens) 1] focal)
          (update boxes idx conj [label focal]))
        (assoc boxes idx [[label focal]])))
    (.contains op "-")
    (let [[label] (str/split op #"-")
          idx (hash label)]
      (assoc boxes idx (vec (remove #(= (first %) label) (boxes idx)))))
    :else boxes))

(defn main []
  (let [input (read-file "input.txt")
        init-seqs (str/split (first input) #",")]
    (->> init-seqs
         (reduce update-boxes {})
         (map (fn [[box lenses]]
                (->> lenses
                     (map-indexed 
                      (fn [idx lens]
                        (* (+ 1 idx)
                           (+ 1 box)
                           (+ (second lens)))))
                     (reduce +))))
         (reduce +))
    ))
