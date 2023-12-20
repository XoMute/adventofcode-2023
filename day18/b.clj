(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map (fn [^String s] (.trim s)) (line-seq rdr)))))

(defn parse-move [^String line]
  (let [[_ _ ^String code] (str/split line #"\s+")
        code (.substring code 1 (dec (.length code)))]
    [(case (last code)
       \0 :R
       \1 :D
       \2 :L
       \3 :U)
     (bigint (read-string (str "0x" (.substring code 1 (dec (.length code))))))]))

;; Returns [[x1 y1], [x2 y2], ...]
(defn to-coordinates [moves]
  (let [n (count moves)]
    (letfn [(inner [[coords is-outer?] i]
              (let [[prev-x prev-y] (last coords)
                    [move step] (nth moves i)
                    [next-move _] (nth moves (mod (+ i 1) n))
                    [prev-move _] (nth moves (or (and (>= (- i 1) 0) ;; special case for the first item. How to get rid of it?
                                                      (- i 1))
                                                 (- n 1)))]
                (case move
                  :R (if (= next-move prev-move)
                       [(conj coords [(+ prev-x step) prev-y]) (not is-outer?)]
                       (let [next-x (if is-outer?
                                      (+ prev-x (inc step))
                                      (+ prev-x (dec step)))]
                         [(conj coords [next-x prev-y]) is-outer?]))
                  :L (if (= next-move prev-move)
                       [(conj coords [(- prev-x step) prev-y]) (not is-outer?)]
                       (let [next-x (if is-outer?
                                      (- prev-x (inc step))
                                      (- prev-x (dec step)))]
                         [(conj coords [next-x prev-y]) is-outer?]))
                  :U (if (= next-move prev-move)
                       [(conj coords [prev-x (- prev-y step)]) (not is-outer?)]
                       (let [next-y (if is-outer?
                                      (- prev-y (inc step))
                                      (- prev-y (dec step)))]
                         [(conj coords [prev-x next-y]) is-outer?]))
                  :D (if (= next-move prev-move)
                       [(conj coords [prev-x (+ prev-y step)]) (not is-outer?)]
                       (let [next-y (if is-outer?
                                      (+ prev-y (inc step))
                                      (+ prev-y (dec step)))]
                         [(conj coords [prev-x next-y]) is-outer?])))))]
      (first (reduce inner
                     [[[0 0]] true] ;; False is hard-coded for my specific input
                     (range n))))))

(defn area [polygon]
  (let [n (count polygon)]
    (abs (quot 
          (reduce (fn [area i]
                    (let [[prev-x prev-y] (nth polygon (or (and (>= (- i 1) 0) ;; special case for the first item. How to get rid of it?
                                                                (- i 1))
                                                           (- n 1)))
                          [x y] (nth polygon i)]
                      (+ area (* (+ prev-x x) (- prev-y y)))))
                  0
                  (range n))
          2))))

(defn main []
  (let [lines (read-file "input.txt")]
    (->> lines
         (mapv parse-move)
         to-coordinates
         area)))
