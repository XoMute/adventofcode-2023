(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn palindrome? [^String s]
  (->> (range (/ (.length s) 2))
       (every? #(= (.charAt s %) (.charAt s (- (.length s) % 1))))))

(defn transpose [xs]
  (apply map str xs))

(defn vertical-reflection-line [pattern]
  ;; TODO: memoization
  (letfn [(inner [start end]
            (cond
              (<= end start) nil
              (< (- end start) 2) nil
              (every? #(palindrome? (.substring % start end)) pattern)
              (quot (+ end start) 2)
              :else (->> (range 1 end)
                         (some (fn [x]
                                 (or (and (>= (- end x) 2) 
                                          (= (mod (+ end x) 2) 0)
                                          (every? #(palindrome? (.substring % x end)) pattern)
                                          (quot (+ end x) 2))
                                     (and (>= (- x start) 2) 
                                          (= (mod (+ x start) 2) 0)
                                          (every? #(palindrome? (.substring % start x)) pattern)
                                          (quot (+ x start) 2))))))))]
    (inner 0 (.length (first pattern)))))

(defn horizontal-reflection-line [pattern]
  (let [pattern (transpose pattern)]
    (vertical-reflection-line pattern)))

(defn solve-pattern [pattern]
  (let [vertical-reflection (vertical-reflection-line pattern)
        horizontal-reflection (horizontal-reflection-line pattern)]
    [vertical-reflection horizontal-reflection]))

(defn main []
  (let [lines (read-file "input.txt")
        patterns (->> lines
                     (partition-by #(not (empty? %)))
                     (remove #(empty? (first %))))
  ]
    (->> patterns
         (map solve-pattern)
         (map #(+ (or (first %) 0) (* (or (second %) 0) 100)))
         (reduce +)
         )))
