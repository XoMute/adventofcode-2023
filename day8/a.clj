(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

;; This function hides information about correct parsing of the file
(defn parse-input [lines]
  (letfn [(parse-node [line]
            (let [[node nodes] (str/split line #"\s+=\s+")
                  nodes (re-seq #"[A-Z]+" nodes)]
              [node nodes]))]
    (let [instructions (map #(if (= % \L) 0 1) (first lines))
          nodes (map parse-node (rest (rest lines)))]
      [instructions nodes])))

(defn solve [instructions nodes]
  (loop [instructions instructions
         node (first (filter #(= "AAA" (first %)) nodes))
         counter 0]
    (if (= (first node) "ZZZ")
      counter
      (let [next-node (nth (second node) (first instructions))]
        (recur (rest instructions) 
               (first (filter #(= (first %) next-node) nodes))
               (+ counter 1))))))

(defn main []
  (let [lines (read-file "input.txt")
        [instructions nodes] (parse-input lines)]
    (solve (->> instructions
               repeat
               (apply concat))
           nodes)))
