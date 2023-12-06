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


(defn main []
  (let [lines (read-file "input.txt")
        [time distance] (apply parse-race lines)
        loosing-times (->> time
                           (range)
                           (take-while #(<= (* % (- time %))
                                            distance))
                           count)]
    (+ (- time (* 2 loosing-times)) 1)))

