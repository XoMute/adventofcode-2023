(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

;; returns [(num x1 x2 y)+]
(defn parse-numbers [idx line]
  (letfn [(parse-number [n start-from]
            (let [x1 (str/index-of line n start-from)]
              (when x1
                (let [x2 (- (+ x1 (.length n)) 1)]
                  (if (and (or (= x1 0)
                               (not (Character/isDigit (.charAt line (- x1 1)))))
                           (or (= x2 (- (.length line) 1))
                               (not (Character/isDigit (.charAt line (+ x2 1))))))
                    (cons [(Integer/parseInt n) x1 x2 idx]
                          ;; TODO: reuse x2 here?
                          (parse-number n (+ x1 (.length n))))
                    (parse-number n (+ x1 (.length n))))))))]
    (->> line
         (re-seq #"\d+")
         distinct
         (mapcat #(parse-number % 0)))))

;; returns [(sign x y)+]
(defn parse-signs [idx line]
  (letfn [(is-sign? [ch]
            (and (not= ch \.)
                 (not (Character/isDigit ch))))]
    (let [signs
          (->> line
               (reduce
                (fn [[x acc] ch]
                  (if (is-sign? ch)
                    [(+ x 1) (conj acc [ch x])]
                    [(+ x 1) acc]))
                [0 ()])
               second)]
      (map #(conj % idx) signs))))

(defn adjacent-sign? [[_ x1 x2 y] [_ sx sy]]
  (and (>= sx (- x1 1))
       (<= sx (+ x2 1))
       (>= sy (- y 1))
       (<= sy (+ y 1))))

(defn adjacent? [number signs]
  (when (seq (filter #(adjacent-sign? number %) signs))
    number))

;; Looks like this method cannot be reused and can be inlined in the code. `adjacent?` looks like a much better abstraction.
;; (defn numbers-adjacent-to-signs [numbers signs]
;;   (filter adjacent? numbers))

(defn main []
  (let [lines (read-file "input.txt")
        numbers (->> lines 
                     (map-indexed parse-numbers)
                     (apply concat))
        signs (->> lines
                   (map-indexed parse-signs)
                   (apply concat))]
    (->> (filter #(adjacent? % signs) numbers)
         (map first)
         (reduce +))))

