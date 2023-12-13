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
              (every? (fn [^String s] (palindrome? (.substring s start end))) pattern)
              (quot (+ end start) 2)
              :else (->> (range 1 end)
                         (map (fn [x]
                                 (or (and (>= (- end x) 2) 
                                          (= (mod (+ end x) 2) 0)
                                          (every? (fn [^String s] (palindrome? (.substring s x end))) pattern)
                                          (quot (+ end x) 2))
                                     (and (>= (- x start) 2) 
                                          (= (mod (+ x start) 2) 0)
                                          (every? (fn [^String s] (palindrome? (.substring s start x))) pattern)
                                          (quot (+ x start) 2)))))
                         (remove false?)
                         seq)))]
    (inner 0 (.length (first pattern)))))

(defn horizontal-reflection-line [pattern]
  (let [pattern (transpose pattern)]
    (vertical-reflection-line pattern)))

(defn flip-at [^String s idx]
  (let [ch (.charAt s idx)
        new-ch (if (= ch \.)
                 \#
                 \.)]
    (str (.substring s 0 idx) new-ch (.substring s (+ idx 1)))))

(defn solve-pattern [pattern]
  (let [original-vertical-reflection (first (vertical-reflection-line pattern))
        original-horizontal-reflection (first (horizontal-reflection-line pattern))]
    #_(prn "orig vert" original-vertical-reflection "orig hor" original-horizontal-reflection)
    (->> (mapcat
          (fn [y]
            (map
             (fn [x]
               (let [changed-pattern (map-indexed #(if (= %1 y) 
                                                     (flip-at %2 x)
                                                     %2) pattern)
                     new-vertical-reflections (vertical-reflection-line changed-pattern)
                     new-horizontal-reflections (horizontal-reflection-line changed-pattern)]
                 #_(prn "row" y "column" x "new vert" new-vertical-reflection "new hor" new-horizontal-reflection)
                 (cond
                   (and (not original-vertical-reflection) 
                        new-vertical-reflections)
                   [(first new-vertical-reflections) nil]
                   (and (not original-horizontal-reflection)
                        new-horizontal-reflections)
                   [nil (first new-horizontal-reflections)]
                   (> (count new-vertical-reflections) 1)
                   [(first (filter #(not= % original-vertical-reflection) new-vertical-reflections)) nil]
                   (> (count new-horizontal-reflections) 1)
                   [nil (first (filter #(not= % original-horizontal-reflection) new-horizontal-reflections))]
                   (and (first new-vertical-reflections)
                        (not= (first new-vertical-reflections) original-vertical-reflection))
                   [(first new-vertical-reflections) nil]
                   (and (first new-horizontal-reflections)
                        (not= (first new-horizontal-reflections) original-horizontal-reflection))
                   [nil (first new-horizontal-reflections)])))
             (range (count (first pattern)))))
          (range (count pattern)))
         (remove #(every? nil? %))
         first)
    
    #_    [original-vertical-reflection original-horizontal-reflection]))

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
