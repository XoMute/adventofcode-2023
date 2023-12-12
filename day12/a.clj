(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn parse-line [line]
  (let [[line groups] (str/split line #"\s+")]
    [line (map #(Integer/parseInt %) (str/split groups #","))]))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn conforms? [original-line groups]
  (let [idx-max (count original-line)
        line (str original-line \.)]
    (prn "checking line" original-line)
    (letfn [(inner [group-count groups ch idx]
              ;; (prn ">> inner")
              ;; (prn group-count)
              ;; (prn groups)
              ;; (prn line)
              ;; (prn idx)
              (if (= idx idx-max)
                (if (> group-count 0)
                  (if (= (count groups) 1)
                    (if (= group-count (first groups))
                      (do
                                        ;(prn "Winning line" line "groups" groups "group-count" group-count)
                        1)
                      0)
                    0)
                  (if (empty? groups)
                    (do
                                        ;(prn "Winning line" line)
                      1)
                    0))
                ;; optimization for early end of algorithm
              #_(if (and (empty? groups)
                         (> group-count 0))
                  0)
                (case ch
                  \. (if (> group-count 0)
                       (if (= group-count (or (first groups) 0))
                         (recur 0 (rest groups) (nth line (inc idx)) (inc idx))
                         0)
                       (recur 0 groups (nth line (inc idx)) (inc idx)))
                  \# (if (< group-count (or (first groups) 0))
                       (recur (inc group-count) groups (nth line (inc idx)) (inc idx))
                       0)
                  \? (+ (inner group-count groups \# idx)
                        (inner group-count groups \. idx)))))]
      (inner 0 groups (first line) 0))))

(defn main []
  (let [lines (read-file "input.txt")
        lines-with-groups (map parse-line lines) 
  ]
    (->> lines-with-groups
         (map #(apply conforms? %))
         (reduce +))))
