(defn transpose-string [st]
    (let [to-array (string/split "\n" st)
          reversed-arr (map tuple ;to-array)
          to-bytes (map |(map string/from-bytes $) reversed-arr)
          joined (map string/join to-bytes)
          res (string/join joined "\n")]
    res))

(defn compare-str [s1 s2]
    (var res 0)
    (let [s1b (string/bytes s1)
          s2b (string/bytes s2)
          len (length s1b)]
        (loop [i :range [0 len]]
            (if (not= (s1b i) (s2b i)) (++ res))))
            res)

(defn get-symmetry [t diff]
    (var res 0)
    (let [len (length t)]
        (loop [i :range [1 len]]
            (def size (min i (- len i)))
            (def fst (array/slice t (- i size) i))
            (def lst (array/slice t i (+ i size)))
            (def fst-string (string/join fst))
            (def lst-string  (string/join (reverse lst)))
            (if (= (compare-str fst-string lst-string) diff) (set res i))
            ))
        res
    )

(defn compare-symmetry [t diff]
    (var res 0)
    (let [sliced (string/split "\n" t)
          ts (string/split "\n" (transpose-string t))]
        (set res (* 100 (get-symmetry sliced diff)))
        (+= res (get-symmetry ts diff)))
    res)

(defn main [&]
    (let [input (string/split "\n\n" (string/trim (slurp "inputs/day13")))
        transposed (map transpose-string input)]
    (print "Day13 : " (sum (map |(compare-symmetry $ 0) input)) " " (sum (map |(compare-symmetry $ 1) input)))))