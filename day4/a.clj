(use 'clojure.java.io)
(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

;; returns [card-id [winning-numbers] [present-numbers]]
(defn parse-card [line]
  (let [[card numbers] (str/split line #":\s+")
        [_ card-id] (str/split card #"\s+")
        [winning-numbers present-numbers] (str/split numbers #"\s+\|\s+")
        parse-nums (fn [nums] (->> (str/split nums #"\s+")
                                   (map #(Integer/parseInt %))
                                   set))]
    [card-id (parse-nums winning-numbers) (parse-nums present-numbers)]))

(defn card-points [[card-id winning-numbers present-numbers]]
  (let [n (count (set/intersection winning-numbers present-numbers))]
    (if (> n 0)
      (int (Math/pow 2 (- n 1)))
      0)))

(defn main []
  (let [lines (read-file "input.txt")
        cards (->> lines 
                   (map parse-card))]
    (->> cards
         (map card-points)
         (reduce +))))

