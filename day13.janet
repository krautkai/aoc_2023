(defn transpose-string [st]
    (let [to-array (string/split "\n" st)
          reversed-arr (map tuple ;to-array)
          to-bytes (map |(map string/from-bytes $) reversed-arr)
          joined (map string/join to-bytes)
          res (string/join joined "\n")]
    res))

(defn get-symmetry [t]
    (var res 0)
    (let [len (length t)]
        (loop [i :range [1 len]]
            (def size (min i (- len i)))
            (def fst (array/slice t (- i size) i))
            (def lst (array/slice t i (+ i size)))
            (def fst-string (string/join fst))
            (def lst-string  (string/join (reverse lst)))
            (if (= fst-string lst-string) (set res i))
            ))
        res
    )

(defn compare-symmetry [t]
    (var res 0)
    (let [sliced (string/split "\n" t)
          ts (string/split "\n" (transpose-string t))]
        (set res (* 100 (get-symmetry sliced)))
        (if (= 0 res) (set res (get-symmetry ts)))
        )
    res)

(defn main [&]
    (let [input (string/split "\n\n" (string/trim (slurp "inputs/day13")))
        transposed (map transpose-string input)]
    (print (sum (map compare-symmetry input)))
    ))