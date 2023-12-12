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

(defn count-pipes-in-chain [pipes chain [y x]]
  (if (seq (filter #(= (second %) [y x]) chain)) ;; TODO: remove if?
    [0 0 0 0]
    (let [up (range y)
          down (range (+ y 1) (count pipes))
          left (range x)
          right (range (+ x 1) (count (first pipes)))]
      [(->> up
            (map (fn [new-y]
                   (first (filter (fn [[_ [ny nx]]] (= [ny nx] [new-y x])) chain))))
            (remove nil?)
            count)
       (->> down
            (map (fn [new-y]
                   (first (filter (fn [[_ [ny nx]]] (= [ny nx] [new-y x])) chain))))
            (remove nil?)
            count)
       (->> left
            (map (fn [new-x]
                   (first (filter (fn [[_ [ny nx]]] (= [ny nx] [y new-x])) chain))))
            (remove nil?)
            count)
       (->> right
            (map (fn [new-x]
                   (first (filter (fn [[_ [ny nx]]] (= [ny nx] [y new-x])) chain))))
            (remove nil?)
            count)])))

;; Returns count of straight pipes in chain from starting position
(defn count-straight-pipes-in-chain [pipes chain [y x]]
  (if (seq (filter #(= (second %) [y x]) chain)) ;; TODO: remove if?
    [0 0 0 0]
    (let [up (range y)
          down (range (+ y 1) (count pipes))
          left (range x)
          right (range (+ x 1) (count pipes))]
      [(->> up
            (map (fn [new-y]
                   (first (filter (fn [[pipe [ny nx]]] (and (= pipe \-) 
                                                            (= [ny nx] [new-y x]))) chain))))
            (remove nil?)
            count)
       (->> down
            (map (fn [new-y]
                   (first (filter (fn [[pipe [ny nx]]] (and (= pipe \-) 
                                                            (= [ny nx] [new-y x]))) chain))))
            (remove nil?)
            count)
       (->> left
            (map (fn [new-x]
                   (first (filter (fn [[pipe [ny nx]]] (and (= pipe \|) 
                                                            (= [ny nx] [y new-x]))) chain))))
            (remove nil?)
            count)
       (->> right
            (map (fn [new-x]
                   (first (filter (fn [[pipe [ny nx]]] (and (= pipe \|) 
                                                            (= [ny nx] [y new-x]))) chain))))
            (remove nil?)
            count)])))

(defn collect-chain-from-point [pipes sx sy]
  (loop [coords (reverse (next-coords-for-start pipes sx sy))
         prev-coords [sy sx]
         chain [[(get-in pipes prev-coords) prev-coords]]
         visited (assoc-in (vec (repeat (count pipes) (vec (repeat (count (first pipes)) false))))
                           prev-coords true)]
    (let [pipe (get-in pipes coords)
          next-coords (reverse (next-coords pipe coords prev-coords))] 
      (if (seq next-coords)
        (if (get-in visited next-coords) ;; TODO: simply check if next pipe is S
          (conj chain [pipe coords])
          (recur next-coords coords
                 (conj chain [pipe coords])
                 (assoc-in visited coords true)))
        chain))))

(defn starting-point [pipes]
  (let [[sy sx] 
        (->> pipes
             (map-indexed (fn [y line]
                            (vector y (.indexOf line \S))))
             (filter #(>= (second %) 0))
             first)]
    [sx sy]))

(defn point-inside-of-chain? [pipes chain point]
  (let [straight-pipes-in-chain (count-straight-pipes-in-chain pipes chain point)
        pipes-in-chain (count-pipes-in-chain pipes chain point)
        ]


    (not-every? (fn [[a b]]
                     (or (= 0 a)
                         (= (mod b 2) 0)))
            [pipes-in-chain straight-pipes-in-chain])))

(defn main []
  (let [lines (read-file "input.txt")
        pipes (lines-to-map lines)
        [sx sy] (starting-point pipes)
        chain (collect-chain-from-point pipes sx sy)
        ]
    (->> (mapcat (fn [y]
                  (filter (fn [x]
                         (point-inside-of-chain? pipes chain [y x]))
                       (range (count (first lines)))))
                (range (count lines)))
         count)
 #_   (prn (count (filter (fn [[_ [y x]]] (= y 1)) chain)))
#_    (point-inside-of-chain? pipes chain [1 0])))
