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
      energized)))

(defn main []
  (let [lines (read-file "input.txt")
        energized (solve lines [0 -1] [0 0]) ]

    (->> energized
         (map (fn [line] (count (filter seq line))))
         (reduce +))))
