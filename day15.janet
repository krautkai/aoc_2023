(defn aoc-hash [t]
    (var res 0)
    (def asciied  (string/bytes t))
    (each as asciied
        (set res (% (* (+ res as) 17) 256)))
        res)

(defn proceed [box el]
    (def isequal (nil?(string/find "-" el)))
    (if (true? isequal)
    (let [params (string/split "=" el)
          lab (params 0)
          len (params 1)
          boxn (aoc-hash (params 0))
          arr (get box boxn)]
        (if (nil? arr)
            (set (box boxn) @[@{:label lab :lens len}])
            (let [idx (find-index |(= ($ :label) (params 0)) arr)]
            (if (not(nil? idx))
                (and(array/remove (box boxn) idx)
                (array/insert (box boxn) idx @{:label lab :lens len}))
                (array/push (box boxn) @{:label lab :lens len})))))
    (let [params (string/split "-" el)
          lab (params 0)
          boxn (aoc-hash lab)
          arr (get box boxn)]
        (if (not(nil? arr))
            (let [idx (find-index |(= ($ :label) lab) arr)]
        (if (not(nil? idx)) (array/remove (box boxn) idx))
        ))))
    box)

(defn focusing-power [xs]
    (var res 0)
    (loop [[box lens] :pairs xs]
        (var i 1)
        (each el lens
            (+= res (* (+ box 1) i (scan-number(el :lens))) )
            (+= i 1)))
    res)

(defn main [&]
    (let [input (string/split ","(string/trim (slurp "inputs/day15")))]
    (print (sum (map aoc-hash input)))
    (print (focusing-power (reduce proceed @{} input)))))
