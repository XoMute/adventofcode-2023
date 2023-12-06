(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn parse-races [time-line distance-line]
  (let [times (second (str/split time-line #":\s+"))
        distances (second (str/split distance-line #":\s+"))
        parsed-times (map bigint (str/split times #"\s+"))
        parsed-distances (map bigint (str/split distances #"\s+"))]
    (map vector parsed-times parsed-distances)))


(defn main []
  (let [lines (read-file "input.txt")
        races (apply parse-races lines)]
    (->> races
         (map (fn [[time distance]]
           (->> time
                (range 1)
                (map #(* % (- time %)))
                (filter #(> % distance))
                count)))
         (reduce *))))

