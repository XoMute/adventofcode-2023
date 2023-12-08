(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn lcm [& xs]
  (let [gcd (fn gcd [a b] (if (= 0 b) a (gcd b (mod a b))))
        lcm (fn lcm [a b] (/ (* a b) (gcd a b)))]
    (reduce lcm xs)))

(defn parse-input [lines]
  (letfn [(parse-node [line]
            (let [[node nodes] (str/split line #"\s+=\s+")
                  nodes (re-seq #"[A-Z0-9]+" nodes)]
              [node nodes]))]
    (let [instructions (map #(if (= % \L) 0 1) (first lines))
          nodes (into {} (map parse-node (rest (rest lines))))]
      [instructions nodes])))

;; Algorightm:
;; 1. Find minimal number of steps for each node to reach the first **Z node
;; 2. Find the LCM of these numbers
(defn solve [instructions nodes]
  (letfn [(reach-to-z [instructions counter node]
            (let [next-node (nth (second node) (first instructions))]
              (if (= (.charAt next-node 2) \Z)
                (+ counter 1)
                (recur (rest instructions) (+ 1 counter)
                       (first (filter #(= (first %) next-node) nodes))))))]
    (let [starting-nodes (filter #(= \A (.charAt (first %) 2)) nodes)]
      (->> starting-nodes
           (map #(reach-to-z instructions 0 %))))))

(defn main []
  (let [lines (read-file "input.txt")
        [instructions nodes] (parse-input lines)]
    (apply lcm
           (solve (->> instructions
                       repeat
                       (apply concat))
                  nodes))))
