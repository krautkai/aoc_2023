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

(defn same-elems [xs]
    (if (empty? xs) true
    (let [fst (first xs)]
        (all |(= fst $)xs)
    )))

(defn all-zeros [xs]
    (all zero? xs))

(defn get-next [xs]
    (var res 0)
    (def df (diff xs))
    (if (all-zeros df) (set res (last xs))
        (set res (+ (get-next df) (last xs))))
    res)

(defn main [&]
    (let [[input] (peg/match parser (slurp "inputs/day9"))]
    (pp (sum(map get-next (input :lines))))))