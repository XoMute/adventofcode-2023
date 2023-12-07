(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(def card-strength
  (into {} (map vector [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range))))

(def type-strength
  {:five 6
   :four 5
   :full-house 4
   :three 3
   :two-pair 2
   :one-pair 1
   :high-card 0})

(defn read-hand [line]
  (let [[hand bid] (str/split line #"\s+")]
    [hand (bigint bid)]))

(defn hand-type [hand]
  (let [sorted (sort hand)
        unique (->> sorted
                    (group-by identity)
                    (map (fn [[k v]] [k (count v)])))]
    (cond
      (seq (filter #(= 5 (second %)) unique)) :five
      (seq (filter #(= 4 (second %)) unique)) :four
      (and (seq (filter #(= 3 (second %)) unique))
           (seq (filter #(= 2 (second %)) unique))) :full-house
      (seq (filter #(= 3 (second %)) unique)) :three
      (= 2 (count (filter #(= 2 (second %)) unique))) :two-pair
      (= 1 (count (filter #(= 2 (second %)) unique))) :one-pair
      :else :high-card)))

(defn compare-cards [card1 card2]
  (compare (card-strength card1) (card-strength card2)))

(defn compare-hands [hand1 hand2]
  (let [hand1 (first hand1)
        hand2 (first hand2)        
        type1 (type-strength (hand-type hand1))
        type2 (type-strength (hand-type hand2))]
    (cond
      (> type1 type2) hand1
      (< type1 type2) hand2
      :else (let [comparisons (map compare-cards hand1 hand2)]
              (reduce
               (fn [acc item]
                 (cond
                   (= acc 1) 1
                   (= acc -1) -1
                   :else item))
               0
               comparisons)))))

(defn compare-hands-2 [hand1 hand2]
  (let [hand1 (first hand1)
        hand2 (first hand2)
        type1 (type-strength (hand-type hand1))
        type2 (type-strength (hand-type hand2))]
    (or (< type1 type2)
        (and (= type1 type2)
             (let [comparisons (map compare-cards hand1 hand2)
                   card-order (reduce
                               (fn [acc item]
                                 (if (= acc 0) item acc))
                               0
                               comparisons)]
               (if (= card-order -1) true false))))))

(defn main []
  (let [lines (read-file "input.txt")
        hands (map read-hand lines)]
    (->> hands
         (sort compare-hands-2)
         (map-indexed #(* (+ %1 1) (second %2)))
         (reduce +))))
