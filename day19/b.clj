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

(defn merge-conditions [cond1 cond2]
  (cond
    (not (seq cond1)) cond2
    (not (seq cond2)) cond1
    :else (merge-with into cond1 cond2)))

(defn invert-rule [{:keys [key op val next] :as rule}]
  (if op
    (if (= op #'<)
      {:key key :op #'> :val (dec val) :next next}
      {:key key :op #'< :val (inc val) :next next})
    rule))

(defn into-conditions [conditions rule]
  (if (contains? rule :key)
    (merge-conditions conditions {(:key rule) [[(:op rule) (:val rule)]]})
    conditions ;; TODO: what to do here?
    ))

(declare inner)

(defn solve [workflows]
  (letfn [(resolve-next-workflow [name conditions]
            (case name
              :accept conditions ;; Given conditions will result in accept
              :reject nil ;; Given conditions will result in reject
              (inner name conditions) ;; Process next rule with current conditions
              ))
          (solve-for-rules [[rule & rules] conditions]
            (if (contains? rule :key)
              [:or 
               (resolve-next-workflow (:next rule) (into-conditions conditions rule))
               (solve-for-rules rules (into-conditions conditions (invert-rule rule)))]
              (resolve-next-workflow (:next rule) conditions)))
          (inner [name conditions]
            (let [[_ rules] (first (filter #(= (first %) name) workflows))]
              (solve-for-rules rules conditions)))]
    (inner "in" '{x [] m [] a [] s []})))

(defn count-distinct-in-single [condition]
  (->> condition
       (map (fn [[_ v]]
              (let [all-less (filter #(= (first %) #'<) v)
                    less (first (sort-by second all-less))
                    all-greater (filter #(= (first %) #'>) v)
                    greater (last (sort-by second all-greater))]
                (cond
                  (and less greater) (- (second less) (second greater) 1)
                  less (dec (second less))
                  greater (- 4000 (second greater))
                  :else 4000))))
       (reduce *)))

(defn count-distinct [conditions]
  (cond
    (= (first conditions) :or)
    (+ (count-distinct (second conditions))
       (count-distinct (last conditions)))
    (nil? conditions) 0
    :else (count-distinct-in-single conditions)))

(defn main []
  (let [lines (read-file "input.txt")
        [workflows] (parse-input lines)]
    (count-distinct (solve workflows))))
