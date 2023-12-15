(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn read-file [file]
  (with-open [rdr (reader file)]
    (reduce conj [] (map #(.trim %) (line-seq rdr)))))

(defn hash [^String str]
  (reduce
   (fn [value char]
     (mod (* (+ value (int char)) 17) 256))
   0
   str))

(defn main []
  (let [input (read-file "input.txt")
        init-seqs (str/split (first input) #",")]

    (reduce + (map hash init-seqs)
            )))
