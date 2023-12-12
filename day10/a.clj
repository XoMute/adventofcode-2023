(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn lines-to-map [lines]
  (let [lines-with-dots (map #(concat (cons \. %) [\.]) lines)
        dotted-line (repeat (count (first lines-with-dots)) \.)
        dotted-lines (concat (cons dotted-line lines-with-dots) [dotted-line])]
    (->> dotted-lines
         (map vec)
         vec
         doall)))

(defn next-coords-for-start [pipes x y]
  (or (and (#{\| \7 \F} (get-in pipes [(- y 1) x])) [x (- y 1)])
      (and (#{\- \J \7} (get-in pipes [y (+ x 1)])) [(+ x 1) y])
      (and (#{\- \F \L} (get-in pipes [y (- x 1)])) [(- x 1) y])
      (and (#{\| \L \J} (get-in pipes [(+ y 1) x])) [x (+ y 1)])))

(defn next-coords [pipe [y x] [prev-y prev-x]]
  (case pipe
;;TODO: fill the map with dots on all sides
    \| (and (= prev-x x) (or (and (< prev-y y) [x (+ y 1)])
                             (and (> prev-y y) [x (- y 1)])))
    \- (and (= prev-y y) (or (and (< prev-x x) [(+ x 1) y])
                             (and (> prev-x x) [(- x 1) y])))
    \L (or (and (< prev-y y) (= prev-x x) [(+ x 1) y])
           (and (= prev-y y) (> prev-x x) [x (- y 1)]))
    \J (or (and (= prev-x x) (< prev-y y) [(- x 1) y])
           (and (= prev-y y) (< prev-x x) [x (- y 1)]))
    \7 (or (and (= prev-y y) (< prev-x x) [x (+ y 1)])
           (and (= prev-x x) (> prev-y y) [(- x 1) y]))
    \F (or (and (= prev-y y) (> prev-x x) [x (+ y 1)])
           (and (= prev-x x) (> prev-y y) [(+ x 1) y]))
    []))

(defn collect-chain-from-point [pipes sx sy]
  (loop [coords (reverse (next-coords-for-start pipes sx sy))
         prev-coords [sy sx]
         chain [(get-in pipes prev-coords)]
         visited (assoc-in (vec (repeat (count pipes) (vec (repeat (count (first pipes)) false))))
                           prev-coords true)]
    (let [pipe (get-in pipes coords)
          next-coords (reverse (next-coords pipe coords prev-coords))] 
      (if (seq next-coords)
        (if (get-in visited next-coords)
          (conj chain pipe)
          (recur next-coords coords
                 (conj chain pipe)
                 (assoc-in visited coords true)))
        ;; TODO: handle error of wrong pipes
        chain))))

(defn starting-point [pipes]
  (let [ ;distances (transient (vec (repeat (count pipes) (vec (repeat (count (first pipes)) 0))))) 
        [sy sx]  (->> pipes
                      (map-indexed (fn [y line]
                                     (vector y (.indexOf line \S))))
                      (filter #(>= (second %) 0))
                      first)]
    
    #_(persistent! distances)
    [sx sy]))

(defn main []
  (let [lines (read-file "input.txt")
        pipes (lines-to-map lines)
        [sx sy] (starting-point pipes)
        chain (collect-chain-from-point pipes sx sy)
        ]
    (/ (count chain) 2)))
