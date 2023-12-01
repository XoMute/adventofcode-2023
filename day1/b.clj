(use 'clojure.java.io)

(def word-to-digit
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

(defn tails [xs]
  (take-while seq (iterate #(subs % 1) xs)))

(defn read-digits [str]
  (->> str
       tails
       (map #(re-find #"\d|one|two|three|four|five|six|seven|eight|nine" %))
       (remove nil?)
       (map #(or (word-to-digit %) %))))

(defn main []
  (let [lines (read-file "input.txt")
        chars (map read-digits lines)
        first-and-last (map #(vector (first %) (last %)) chars)]
    (->> first-and-last
         (map #(apply str %))
         (map #(Integer/parseInt %))
         (reduce +))))

