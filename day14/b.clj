(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn transpose [xs]
  (apply map str xs))

(defn tilt-line [^String line]
  (let [[_ stop-idx new-line] 
        (reduce
         (fn [[idx current-idx new-line] ch]
           (case ch
             \. [(inc idx) current-idx new-line]
             \# [(inc idx) (inc idx) (str (apply str new-line (repeat (- idx current-idx) \.)) ch)]
             \O [(inc idx) (inc current-idx) (str new-line ch)]))
         [0 0 ""]
         line)]
    (apply str new-line (repeat (- (count line) stop-idx) \.))))

(defn count-line-load [^String line]
  (second (reduce 
           (fn [[idx sum] ch]
             (case ch
               \. [(inc idx) sum]
               \# [(inc idx) sum]
               \O [(inc idx) (+ sum (- (count line) idx))])
             )
           [0 0]
           line)))

(defn plate-load [plate]
  (->> plate
       transpose
       (map count-line-load)
       (reduce +)))

(defn one-cycle [plate]
  (let [north-lines (map tilt-line (transpose plate))
        west-lines (map tilt-line (transpose north-lines))
        south-lines (map tilt-line (map reverse (transpose west-lines)))
        east-lines (map tilt-line (map reverse (transpose (map reverse south-lines))))]
    (map #(apply str %) (map reverse east-lines))))

(defn hash-plate [plate]
  plate
  #_(reduce str plate))

(defn solve [plate]
  (letfn [(inner [previous-plates plate idx]
            (let [next-plate (one-cycle plate)
                  hashed (hash-plate next-plate)
                  prev-state (first (filter #(= (first %) hashed) previous-plates))]
              
              
              (if prev-state
                (do
                  (prn "cycle found!! idx" idx (second prev-state))
                  (prn (+ (second prev-state) ;; loop start
                          (mod (- 1000000000 idx) (- idx (second prev-state)))))                
                  (nth previous-plates
                       (- (+ (second prev-state) ;; loop start
                            (mod (- 1000000000 idx) (- idx (second prev-state))))
                          1)))
                (recur (conj previous-plates [hashed idx]) next-plate (inc idx)))))]
    (inner [] plate 0)))

(defn main []
  (let [lines (read-file "input.txt")
        plate lines
        last-plate (first (solve plate))]
    (doall (map prn last-plate))
    (prn (map count-line-load (transpose last-plate)))
    (plate-load last-plate)

    
    ))
