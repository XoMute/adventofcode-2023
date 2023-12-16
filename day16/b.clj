(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

;; TODO: inline
(defn next-pos [[prev-y prev-x] [cur-y cur-x]]
  (cond 
    ;; go up
    (< cur-y prev-y) [(dec cur-y) cur-x]
    ;; go down
    (> cur-y prev-y) [(inc cur-y) cur-x]
    ;; go left
    (< cur-x prev-x) [cur-y (dec cur-x)]
    ;; go right
    (> cur-x prev-x) [cur-y (inc cur-x)]))

(defn solve [layout prev-pos cur-pos]
  (let [energized (to-array-2d ;; now can use mutable aset and aget 
                   (vec (repeat (count layout) (vec (repeat (count (first layout)) [])))))]
    (letfn [(inner [prev-pos cur-pos]
              (let [[prev-y prev-x] prev-pos
                    [cur-y cur-x] cur-pos]
                (when-not (or
                           (< cur-y 0) ;; check for correct coords
                           (>= cur-y (count layout))
                           (< cur-x 0)
                           (>= cur-x (count (first layout))))
                  (let [prev-beams (aget energized cur-y cur-x)]
                    (when-not ;; check if we already walked here
                        (first (filter #(= prev-pos %) prev-beams))
                        (aset energized cur-y cur-x (conj prev-beams prev-pos))
                        (case (get-in layout cur-pos)
                          \. (recur cur-pos
                                    (next-pos prev-pos cur-pos))
                          \\ (recur cur-pos 
                                    (cond
                                      (< prev-x cur-x) [(inc cur-y) cur-x]
                                      (> prev-x cur-x) [(dec cur-y) cur-x]
                                      (< prev-y cur-y) [cur-y (inc cur-x)]
                                      (> prev-y cur-y) [cur-y (dec cur-x)]))
                          \/ (recur cur-pos 
                                    (cond
                                      (< prev-x cur-x) [(dec cur-y) cur-x]
                                      (> prev-x cur-x) [(inc cur-y) cur-x]
                                      (< prev-y cur-y) [cur-y (dec cur-x)]
                                      (> prev-y cur-y) [cur-y (inc cur-x)]))
                          \- (cond
                               (not= prev-x cur-x) 
                               (recur cur-pos (next-pos prev-pos cur-pos))
                               :else (do (inner cur-pos [cur-y (dec cur-x)])
                                         (recur cur-pos [cur-y (inc cur-x)])))
                          \| (cond
                               (not= prev-y cur-y)
                               (recur cur-pos (next-pos prev-pos cur-pos))
                               :else (do (inner cur-pos [(dec cur-y) cur-x])
                                         (recur cur-pos [(inc cur-y) cur-x])))))))))]
      
      (inner prev-pos cur-pos)
      (->> energized
           (map #(count (filter seq %)))
           (reduce +)))))

(defn main []
  (let [lines (read-file "input.txt")
        max-left (apply max 
                        (map (fn [y]
                               ;; start from left to right
                               (solve lines [y -1] [y 0]))
                             (range (count lines))))
        max-right (apply max 
                         (map (fn [y]
                                ;; start from right to left
                                (solve lines [y (count (first lines))] [y (dec (count (first lines)))]))
                              (range (count lines))))
        max-up (apply max 
                      (map (fn [x]
                             ;; start from up to down
                             (solve lines [-1 x] [0 x]))
                           (range (count lines))))

        max-down (apply max 
                        (map (fn [x]
                               ;; start from down to up
                               (solve lines [(count lines) x] [(dec (count lines)) x]))
                             (range (count lines))))]
    
    (max max-left max-right max-up max-down)
    ))
