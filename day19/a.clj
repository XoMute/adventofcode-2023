(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map (fn [^String s] (.trim s)) (line-seq rdr)))))

(defn parse-input [lines]
  (letfn [(parse-name [^String name]
            (case name
              "A" :accept
              "R" :reject
              name))
          (parse-rule [^String rule]
            ;; Returns {:key "key" :op "op" :val val :next "next"}
            (if (> (.indexOf rule ":") -1)
              (let [[condition next] (str/split rule #":")
                    [_ key op val] (re-find #"^([A-Za-z]+)([<|>])(\d+)$" condition)]
                {:key (read-string key) :op (if (= op ">") #'> #'<)
                 :val (Integer/parseInt val) :next (parse-name next)})
              {:next (parse-name rule)}))
          (parse-workflow [^String line]
            (let [[name rules] (str/split line #"\{")
                  rules (.substring rules 0 (dec (.length rules)))
                  rules (str/split rules #",")]
              [name (mapv parse-rule rules)]))
          (parse-part [^String line]
            (read-string 
             (str/replace line "=" " ")))]
    (let [[workflows _ parts] (partition-by #(not= "" %) lines)]
      [(into {} (map parse-workflow workflows)) (map parse-part parts)])))

;; Returns next workflow name, :accept or :reject
(defn next-workflow [[_ workflow] part]
  (letfn [(inner [[rule & rules]]
            (if (contains? rule :key)
              (let [val (part (:key rule))]
                (if ((:op rule) val (:val rule))
                  (:next rule)
                  (inner rules)))
              (:next rule)))]
    (inner workflow)))

(defn process-part [workflows part]
  (letfn [(inner [workflow-name]
            (let [workflow (first (filter #(= (first %) workflow-name) workflows))
                  next-workflow (next-workflow workflow part)]
              (cond
                (= next-workflow :accept) true
                (= next-workflow :reject) false
                :else (inner next-workflow))))]
    (inner "in")))

(defn main []
  (let [lines (read-file "input.txt")
        [workflows parts] (parse-input lines)]
    (->> parts
         (filter
          (partial process-part workflows))
         (map (fn [part]
                (reduce + (map second part))))
         (reduce +))))
