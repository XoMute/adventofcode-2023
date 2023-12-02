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

(def red-limit 12)
(def green-limit 13)
(def blue-limit 14)

(defn is-possible? [[_ rounds]]
  (every? (fn [[r g b]] 
            (and (<= r red-limit)
                 (<= g green-limit)
                 (<= b blue-limit)))
          rounds))

(defn main []
  (let [lines (read-file "input.txt")]
    (->> lines
         (map parse-line)
         (filter is-possible?)
         (map first)
         (reduce +))))

