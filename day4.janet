(import ./utils/utils :as utl)

(defn card [id winning mine]
  @{:id id
    :winning winning
    :mine mine
    :copies 1})

(def parse-card
  ~{:array (/ (some (* :s* (number :d+) :s*)) ,array)
    :main (/ (* "Card" :s+ (number :d+) ":" :array "|" :array) ,card)})

(defn check-wins [t]
    (var score 0)
    (let [wins (t :winning)
          mine (t :mine)
          inter (utl/intersection wins mine)
          len (length inter)]
        (cond
            (= len 1) (set score 1)
            (> len 1) (set score (math/exp2 (- len 1)))
             ))
            score)

(defn score2 [t mp]
  (let [winners (t :winning)
        id (t :id)
        mine (t :mine)
        copies (t :copies)
        inter (utl/intersection winners mine)
        wins (length inter)]
  (loop [i :range-to [1 wins]] #range-to` -- same as :range, but the range is inclusive [start, end].
    (update (mp (+ id i)) :copies |(+ $ copies))) #(update ds key func & args)
  copies))


(defn main [&]
    (let [str (string/split "\n" (slurp "inputs/day4"))
          parsed (mapcat |(peg/match parse-card $) str) 
          # (mapcat f ind & inds) Map a function over every element in an array or tuple and use `array/concat` to concatenate the results.
          #(short-fn arg &opt nam) Shorthand for `fn`. Arguments are given as `$n`, where `n` is the 0-indexed argument of the function. `$` is also an alias for the first (index 0) argument. The `$&` symbol will make the anonymous function variadic if it appears in the body of the function, and can be combined with positional arguments.
          calcul (map check-wins parsed)
          res1 (sum calcul)
          cardmap (tabseq [c :in parsed] (c :id) c)
          res2 (sum (map |(score2 $ cardmap) parsed))]
          (print res1)
          (print res2)))
