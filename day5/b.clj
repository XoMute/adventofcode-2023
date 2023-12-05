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
    (->> seeds
         (map #(bigint %))
         (partition 2)
         (map (fn [[start n]]
                [start (- (+ start n) 1)])))))

(defn parse-map-entry [line]
  (let [[destination source length] (map bigint (str/split line #"\s"))]
    [destination source (- (+ length source) 1)]))

;; returns fn with clojure on given values
;; Range -> List(Range)
;; The returned ranges are already mapped to correct destinations
(defn parse-map [lines]
  (let [entries (map parse-map-entry (next lines))]
    (fn [[input-start input-end]]
      (let [intersected-entries
            (->> entries 
                 (filter (fn [[_ src-start src-end]]                           
                           (or 
                            ;; check if src range is part of input range
                            (or (and (>= src-start input-start) 
                                     (<= src-start input-end))
                                (and (>= src-end input-start) 
                                     (<= src-end input-end)))
                            ;; check if input range is part of src range
                            (or (and (>= input-start src-start) 
                                     (<= input-start src-end))
                                (and (>= input-end src-start) 
                                     (<= input-end src-end))))))
                 (sort-by second))]
        ;; TODO: check cases if range is 1 num long
        (or (->> intersected-entries
                 (reduce (fn [[[range-start range-end] output-ranges] [dest src-start src-end]]
                           (let [diff (- dest src-start)]
                             (if (<= src-start range-start)
                               ;; src range starts before input range
                               (if (>= src-end range-end)
                                 ;; input range is a subset of src range, finish the evaluation
                                 [[] 
                                  (conj output-ranges [(+ range-start diff) (+ range-end diff)])]
                                 [[(+ 1 src-end) range-end]
                                  (conj output-ranges [(+ range-start diff) (+ src-end diff)])])
                               ;; src range starts inside input range
                               [[src-start range-end]
                                (conj output-ranges 
                                      [range-start (- src-start 1)] ;; first part of input range without changes
                                      [(+ src-start diff)
                                       (if (<= src-end range-end)
                                         ;; src range ends inside input range
                                         (+ src-end diff)
                                         ;; src range ends beyond input range
                                         (+ range-end diff))])])))
                         (list [input-start input-end] []))
                 second
                 seq)
            ;; no intersections at all
            (list [input-start input-end]))))))

(defn main []
  (let [lines (read-file "input.txt")
        seeds-and-maps (->> lines
                            (partition-by #(= % ""))
                            (remove #(= (first %) "")))
        seeds (doall (parse-seeds (ffirst seeds-and-maps)))
        maps (map parse-map (next seeds-and-maps))]
    (->> (reduce (fn [ranges map-fn]
              (mapcat map-fn ranges))
            seeds
            maps)
         (map first)
         sort
         first)))

