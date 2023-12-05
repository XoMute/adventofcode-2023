(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn third [s]
  (nth s 2))

(defn parse-seeds [line]
  (let [seeds (-> line
                  (str/split #":\s")
                  second
                  (str/split #"\s"))]
    (map #(bigint %) seeds)))

(defn parse-map-entry [line]
  (let [[source destination length] (str/split line #"\s")]
    [(bigint source) (bigint destination) (bigint length)]))

;; returns fn with clojure on given values
(defn parse-map [lines]
  (let [entries (map parse-map-entry (next lines))]
    (fn [x]
      (let [entry (->> entries 
               (filter (fn [[_ source n]]
                         (and (>= x source)
                              (< x (+ source n)))))
               first)]
        (if (seq entry)
          (let [[dest source _] entry
                diff (- x source)]
            (+ dest diff))
          x)))))

(defn main []
  (let [lines (read-file "input.txt")
        seeds-and-maps (->> lines
                            (partition-by #(= % ""))
                            (remove #(= (first %) "")))
        seeds (parse-seeds (ffirst seeds-and-maps))
        maps (map parse-map (next seeds-and-maps))]
    (->> seeds
         (map (fn [seed]
                (reduce (fn [acc map] (map acc))
                        seed
                        maps)))
         sort
         first)
    (first maps)))

