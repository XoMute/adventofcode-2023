(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(def card-strength
  (into {} (map vector [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range))))

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

(defn inline-jokers [hand]
  (let [sorted (sort hand)
        unique-with-jokers (->> sorted
                                (group-by identity)
                                (map (fn [[k v]] [k (count v)])))
        jokers (or (first (filter #(= (first %) \J) unique-with-jokers))
                   [\J 0])
        without-jokers (->> unique-with-jokers
                            (remove #(= (first %) \J))
                            (sort-by second)
                            reverse)
        [first-card first-count] (first without-jokers)]
    (if (seq without-jokers)
      (cons [first-card (+ first-count (second jokers))] (rest without-jokers))
      [jokers])))

(defn hand-type [hand]
  (let [unique (inline-jokers hand)]
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

(defn compare-hands-card-by-card [hand1 hand2]
  (cond
    (not (seq hand1)) false
    (= (first hand1) (first hand2)) (compare-hands-card-by-card (rest hand1) (rest hand2))
    :else (= (compare-cards (first hand1) (first hand2)) -1)))

(defn compare-hands [hand1 hand2]
  (let [type1 (type-strength (hand-type hand1))
        type2 (type-strength (hand-type hand2))]
    (or (< type1 type2)
        (and (= type1 type2)
             (compare-hands-card-by-card hand1 hand2)))))

(defn main []
  (let [lines (read-file "input.txt")
        hands (map read-hand lines)]
    (->> hands
         (sort-by first compare-hands)
         (map-indexed #(* (+ %1 1) (second %2)))
         (reduce +))))
