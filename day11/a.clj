(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

;; Returns ([y x]+)
(defn galaxies [space]
  (->> space 
       (map-indexed 
        (fn [y line]
          (->> line
               (map-indexed 
                (fn [x char]
                  (if (= char \#)
                    [y x]
                    [])))
               (filter seq))))
       (filter seq)
       (apply concat)))

;; Returns ([y+], [x+])
(defn empty-rows-and-columns [space]
  [(->> space
        (map-indexed vector) ;; TODO: rewrite to range?
        (filter #(every? (fn [char] (= char \.)) (second %)))
        (map first))
   (->> (range (count (first space)))
        (filter (fn [x]
                  (->> (range (count space))
                       (every? (fn [y] (= \. (get-in space [y x]))))))))])

(defn subsets [n xs]
  (cond
    (= n 0) '(())
    (empty? xs) ()
    :else (concat
           (map #(cons (first xs) %) 
                (subsets (dec n) (rest xs)))
           (subsets n (rest xs)))))

(defn distance [[[y1 x1] [y2 x2]] empty-rows empty-columns]
  (+ (abs (- y2 y1)) 
     (abs (- x2 x1))
     (count (filter #(and (> % (min y1 y2))
                          (< % (max y1 y2))) empty-rows))
     (count (filter #(and (> % (min x1 x2))
                          (< % (max x1 x2))) empty-columns))))

(defn main []
  (let [lines (read-file "input.txt")
        galaxies (galaxies lines)
        [empty-rows empty-columns] (empty-rows-and-columns lines)]
    (->> galaxies
         (subsets 2)
         (map #(distance % empty-rows empty-columns))
         (reduce +))))
