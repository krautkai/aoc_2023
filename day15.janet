(defn aoc-hash [t]
    (var res 0)
    (def asciied  (string/bytes t))
    (each as asciied
        (set res (% (* (+ res as) 17) 256)))
        res)

(defn main [&]
    (let [input (string/split ","(string/trim (slurp "inputs/day15")))]
    (pp input)
    (print (sum (map aoc-hash input)))))