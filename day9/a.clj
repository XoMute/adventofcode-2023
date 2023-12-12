(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn line-to-nums [^String line]
  (vec (map bigint (str/split line #"\s+"))))

(defn solve [nums]
  (if (every? #(= 0 %) nums)
    0
    (let [diff (solve (subvec (first 
                                (reduce (fn [[xs prev-x] x]
                                          [(conj xs (- x prev-x)) x]) 
                                        [[] 0]
                                        nums)) 1))]
      (+ (last nums) diff))))

(defn main []
  (let [lines (read-file "input.txt")
        nums (map line-to-nums lines)]
    (->> nums
         (map solve)
         (reduce +))))
