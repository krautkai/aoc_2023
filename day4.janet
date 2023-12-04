(import ./utils/utils :as utl)

(def grammar
  ~{
    :main (some :games)
    :games (* :card ": " :winners "|" :mine)
    :card (* "Card" (some :s) (<- :number))
    :winners (some (+ :s (<- :number)))
    :mine (some (+ :s (<- :number)))
    :number (some :d)
  })

(defn intersection [t1 t2]
    (var inter @[])
    (each el t1
        (if (has-value? t2 el) (array/push inter el)))
        inter)

(defn check-wins [t]
    (var score 0)
    (let [wins (array/slice t 1 11)
          mine (array/slice t 11 36)
          inter (intersection wins mine)
          len (length inter)]
        (cond
            (= len 1) (set score 1)
            (> len 1) (set score (math/exp2 (- len 1)))
             ))
            score)

(defn main [&]
    (let [str (slurp "inputs/day4")
          parsed (peg/match grammar str)
          split (utl/split-array parsed 36)
          calcul (map check-wins split)
          score (sum calcul)]
          (print score)))
