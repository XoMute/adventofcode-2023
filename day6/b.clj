(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn parse-race [time-line distance-line]
  (let [times (second (str/split time-line #":\s+"))
        distances (second (str/split distance-line #":\s+"))
        parsed-times (bigint (apply str (str/split times #"\s+")))
        parsed-distances (bigint (apply str (str/split distances #"\s+")))]
    [parsed-times parsed-distances]))

(defn bin-search [start end time distance]
  (let [i (quot (+ start end) 2)]
    (cond 
      (>= start end) end
      (> (* i (- time i)) distance) (bin-search start i time distance)
      :else (bin-search (+ i 1) end time distance))))

(defn main []
  (let [lines (read-file "input.txt")
        [time distance] (apply parse-race lines)
        first-greater (bin-search 0 time time distance)]
    (+ (- time (* first-greater 2)) 1)))

