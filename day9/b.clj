(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn line-to-nums [line]
  (map bigint (str/split line #"\s+")))

(defn solve [nums]
  (if (every? #(= 0 %) nums)
    0
    (let [diff (solve (rest (first 
                             (reduce (fn [[xs prev-x] x]
                                       [(conj xs (- x prev-x)) x]) 
                                     [[] 0]
                                     nums))))]
      (- (first nums) diff))))

(defn main []
  (let [lines (read-file "input.txt")
        nums (map line-to-nums lines)]
    (->> nums
         (map solve)
         (reduce +))))
