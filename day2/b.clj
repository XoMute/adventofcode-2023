(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (line-seq rdr))))

(defn parse-color [colors color]
  (let [clr (first (filter #(str/includes? % color) colors))]
    (if clr
      (Integer/parseInt (first (str/split clr #" ")))
      0)))

(defn parse-round [round]
  (let [colors (str/split round #", ")
        red (parse-color colors "red")
        green (parse-color colors "green")
        blue (parse-color colors "blue")]
    [red green blue]))

;; Returns [game-id (red green blue)+]
(defn parse-line [line]
  (let [[game rounds] (str/split line #": ")
        [_ game-id] (str/split game #" ")
        splitted-rounds (str/split rounds #"; ")
        parsed-rounds (map parse-round splitted-rounds)]
    [(Integer/parseInt game-id) parsed-rounds]))

(defn game-power [[_ rounds]]
  (->> rounds
       (reduce (fn [[r1 g1 b1] [r2 g2 b2]]
                 [(max r1 r2) (max g1 g2) (max b1 b2)]) )
       (reduce *)))

(defn main []
  (let [lines (read-file "input.txt")]
    (->> lines
         (map parse-line)
         (map game-power)
         (reduce +))))

