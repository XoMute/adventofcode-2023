(use 'clojure.java.io)

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

(defn digits-from-string [str]
  (filter #(Character/isDigit %) str))

(defn main []
  (let [lines (read-file "input.txt")
        chars (map digits-from-string lines)
        first-and-last (map #(vector (first %) (last %)) chars)]
    (->> first-and-last
         (map #(apply str %))
         (map #(Integer/parseInt %))
         (reduce +))))

