(use 'clojure.java.io)
(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

;; returns [card-id [winning-numbers] [present-numbers] count]
(defn parse-card [line]
  (let [[card numbers] (str/split line #":\s+")
        [_ card-id] (str/split card #"\s+")
        [winning-numbers present-numbers] (str/split numbers #"\s+\|\s+")
        parse-nums (fn [nums] (->> (str/split nums #"\s+")
                                   (map #(Integer/parseInt %))
                                   set))]
    [(Integer/parseInt card-id) (parse-nums winning-numbers) (parse-nums present-numbers) 1]))

(defn n-of-matches [[card-id winning-numbers present-numbers]]
  (count (set/intersection winning-numbers present-numbers)))

(defn copy-next-cards [card-id cards]
  (let [card (first (filter #(= card-id (first %)) cards))
        [card-id _ _ count] card
        matches (n-of-matches card)]
    (->> cards
         (map (fn [card]
                (let [[current-card-id winning present current-count] card]
                  (if (and (> current-card-id card-id)
                           (<= current-card-id (+ card-id matches)))
                    [current-card-id winning present (+ current-count count)]
                    card)))))))

(defn main []
  (let [lines (read-file "input.txt")
        cards (->> lines 
                   (map parse-card))]
    (->> cards
         (reduce (fn [cards [current-card-id _ _ _]]
                   (copy-next-cards current-card-id cards))
                 cards)
         (map #(nth % 3))
         (reduce +))))

