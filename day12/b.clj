(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn parse-line [line]
  (let [[line groups] (str/split line #"\s+")]
    [(apply str (interpose "?" (repeat 5 line)))
     (apply concat (repeat 5 (map #(Integer/parseInt %) (str/split groups #","))))]))

(defn make-conforms? [original-line]
  (let [idx-max (count original-line)
        line (str original-line \.)
        conforms?
        (fn [memo group-count groups ch idx]
          (let [conforms? (fn [group-count groups ch idx] (memo memo group-count groups ch idx))]
            (if (= idx idx-max)
              (if (> group-count 0)
                (if (= (count groups) 1)
                  (if (= group-count (first groups))
                    1
                    0)
                  0)
                (if (empty? groups)
                  1
                  0))
              (case ch
                \. (if (> group-count 0)
                     (if (= group-count (or (first groups) 0))
                       (conforms? 0 (rest groups) (nth line (inc idx)) (inc idx))
                       0)
                     (conforms? 0 groups (nth line (inc idx)) (inc idx)))
                \# (if (< group-count (or (first groups) 0))
                     (conforms? (inc group-count) groups (nth line (inc idx)) (inc idx))
                     0)
                \? (+ (conforms? group-count groups \# idx)
                      (conforms? group-count groups \. idx))))))
        memoized (memoize conforms?)]
    (partial memoized memoized)))

(defn conforms? [line groups]
  ((make-conforms? line) 0 groups (first line) 0))

(defn main []
  (let [lines (read-file "input.txt")
        lines-with-groups (map parse-line lines) 
  ]
    (->> lines-with-groups
         (map #(apply conforms? %))
         (reduce +))))
