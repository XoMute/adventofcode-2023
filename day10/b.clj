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
  (if (seq (filter #(= (second %) [y x]) chain)) ;; Skip the chain itself TODO: remove if?
    0 #_[0 0 0 0]
    (let [horizontal-wall? #{\- \F \J \L \7}
          vertical-wall? #{\| \F \J \L \7}
          up (range y)
          down (range (+ y 1) (count pipes))
          left (range x)
          right (range (+ x 1) (count (first pipes)))]

      #_[(->> up
            (reduce 
             (fn [[prev-angle count] new-y]
               (let [[pipe [ny nx]]
                     (first (filter 
                             (fn [[pipe [ny nx]]] (and (horizontal-wall? pipe) 
                                                       (= [ny nx] [new-y x]))) chain))]
                 (cond
                   (and (= prev-angle \F) (= pipe \J)) [nil count]
                   (and (= prev-angle \L) (= pipe \7)) [nil count]
                   (and (nil? prev-angle)
                        (#{\F \J \L \7} pipe)) [pipe (inc count)]
                   :else [prev-angle (inc count)])))
             [nil 0])
            second
            #_(map (fn [new-y]
                     (reduce (fn [[prev-angle count] [pipe [ny nx]]]
                               (or (and 
                                    (= [ny nx] [new-y x])
                                    (horizontal-wall? pipe)
                                    (cond
                                      (and (= prev-angle \F) (= pipe \J)) [nil count]
                                      (and (= prev-angle \L) (= pipe \7)) [nil count]
                                      (and (nil? prev-angle)
                                           (#{\F \J \L \7} pipe)) [pipe (inc count)]
                                      :else [prev-angle (inc count)]))
                                   [prev-angle count]))
                             [nil 0]
                             chain)))
            #_(#(or (second %) 0)))
       (->> down
            (reduce 
             (fn [[prev-angle count] new-y]
               (let [[pipe [ny nx]]
                     (first (filter 
                             (fn [[pipe [ny nx]]] (and (horizontal-wall? pipe) 
                                                       (= [ny nx] [new-y x]))) chain))]
                 (cond
                   (and (= prev-angle \F) (= pipe \J)) [nil count]
                   (and (= prev-angle \L) (= pipe \7)) [nil count]
                   (and (nil? prev-angle)
                        (#{\F \J \L \7} pipe)) [pipe (inc count)]
                   :else [prev-angle (inc count)])))
             [nil 0])
            second
            #_(#(or (second %) 0)))
       (->> left
            (map (fn [new-x]
                   (first (filter (fn [[pipe [ny nx]]] (and (vertical-wall? pipe) 
                                                            (= [ny nx] [y new-x]))) chain))))
            (remove nil?)
            count)
       (->> right
            (map (fn [new-x]
                   (first (filter (fn [[pipe [ny nx]]] (and (vertical-wall? pipe) 
                                                            (= [ny nx] [y new-x]))) chain))))
            (remove nil?)
            count)]
      (->> up
            (reduce 
             (fn [[prev-angle count] new-y]
               (let [[pipe [ny nx]]
                     (first (filter 
                             (fn [[pipe [ny nx]]] (= [ny nx] [new-y x])) chain))]
                 #_(prn ">> prev-angle" prev-angle "pipe" pipe "count" count) 
                 (cond
                   (or (and (= prev-angle \F) (= pipe \J))
                       #_(and (= prev-angle \L) (= pipe \7))
                       #_(and (= prev-angle \J) (= pipe \F))
                       (and (= prev-angle \7) (= pipe \L)))
                   [nil count]
                   (#{\F \J \L \7} pipe) [pipe (inc count)]
                   (nil? pipe) [nil count]
                   (= pipe \|) [prev-angle count]
                   pipe [prev-angle (inc count)]
                   :else [prev-angle count])))
             [nil 0])
            second))))

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
;; Returns [x y]
(defn starting-point [pipes]
  [62 63]
  #_(let [[sy sx] 
        (->> pipes
             (map-indexed (fn [y line]
                            (vector y (.indexOf line \S))))
             (filter #(>= (second %) 0))
             first)]
    [sx sy]))

(defn point-inside-of-chain? [pipes chain point]
  (let [walls-and-angles-in-chain (count-straight-pipes-in-chain pipes chain point)]

    (if (and (> walls-and-angles-in-chain 0)
             (= (mod walls-and-angles-in-chain 2) 1))
      (prn ">>>>> point" point walls-and-angles-in-chain)
      (prn "point" point walls-and-angles-in-chain))
    (and (> walls-and-angles-in-chain 0)
         (= (mod walls-and-angles-in-chain 2) 1))))

(defn main []
  (let [lines (read-file "input.txt")
        pipes (lines-to-map lines)
        [sx sy] (starting-point pipes)
        chain (collect-chain-from-point pipes sx sy)
        ]
    #_(prn "pipes")
#_    (doall (map prn pipes))
    #_(point-inside-of-chain? pipes chain [11 7])
  
    (->> (mapcat (fn [y]
                   (filter (fn [x]
                             (point-inside-of-chain? pipes chain [y x]))
                           (range (count (first pipes)))))
                 (range (count pipes)))
         count)
    #_   (prn (count (filter (fn [[_ [y x]]] (= y 1)) chain)))
    #_    (point-inside-of-chain? pipes chain [1 0])))
