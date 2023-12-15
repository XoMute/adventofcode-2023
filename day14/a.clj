(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn transpose [xs]
  (apply map str xs))

(defn count-line-load [^String line]
  (second (reduce 
           (fn [[idx sum load] ch]
             #_(prn "idx" idx "sum" sum "load" load "ch" ch)
             (case ch
               \. [(inc idx) sum load]
               \# [(inc idx) sum (- (count line) idx 1)]
               \O [(inc idx) (+ sum load) (dec load)])
             )
           [0 0 (count line)]
           line)))

(defn main []
  (let [lines (read-file "input.txt")
        plate (transpose lines)]
    ;; (doall (map prn plate))
    ;; (prn (count-line-load (nth plate 2)))
    (->> plate
         (map count-line-load)
         (reduce +))
    ))
