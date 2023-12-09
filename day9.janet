(def parser 
    ~{:digits (replace (capture (sequence (opt "-") :d+)),scan-number :digits)
      :line (/ (* (some (+ :digits " "))), array)
      :main (/(some (* :line (any "\n"))), |{:lines $&})})

(defn diff [arr]
    (def res @[])
  (let [len (- (length arr) 1)]
    (loop [i :range [0 len]]
      (array/push res (- (arr (inc i)) (arr i)))))
      res)

(defn get-next [f op xs]
    (var res 0)
    (def df (diff xs))
    (if (all zero? df) (set res (f xs))
        (set res (op (f xs) (get-next f op df))))
    res)

(defn main [&]
    (let [[input] (peg/match parser (slurp "inputs/day9"))]
    (pp (sum (map |(get-next last + $) (input :lines))))
    (pp (sum (map |(get-next first - $) (input :lines))))))