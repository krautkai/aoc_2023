(defn part [l col]
    @{:line (- l 1)
     :col (- col 1)
    })

(def parser 
     ~{:galaxy (/ (* (line) (column)  "#") , part)
       :main (any (* (any (+ "." "\n")) :galaxy))})

(defn update-galaxies [gal emplin empcol coeff]
    (def new-tab @[])
    (each g gal
        (var @{:line line :col col} g)
        (set line (+ line (* coeff (length (filter |( < $ line) emplin)))))
        (set col (+ col (* coeff (length (filter |( < $ col) empcol)))))
        (array/push new-tab {:col col :line line})
    )
    new-tab
    )

(defn manhattan [a b]
    (+ (math/abs (- (a :col) (b :col))) (math/abs (- (a :line) (b :line))) ))

(defn find-empty [t dim]
    (def emptycol @[])
    (def emptylines @[])
    (def filledcol (map |($ :col) t))
    (def filledline (map |($ :line) t))
    (loop [i :range [0 dim]]
        (if (not (has-value? filledcol i)) (array/push emptycol i))
        (if (not (has-value? filledline i)) (array/push emptylines i)))
    {:emptycol emptycol :emptylines emptylines})

(defn get-all-distances [galaxies]
    (def arr @[])
    (def len (length galaxies))
    (loop [i :range [0 (- len 1)]]
        (loop [j :range [(+ i 1) len]]
            (array/push arr (manhattan (galaxies i) (galaxies j)))
        ))
    arr
    )

(defn main [&]
    (let [input (slurp "inputs/day11")
          inp (string/split "\n" input)
          parsed (peg/match parser input)
          {:emptycol emptycol :emptylines emptylines} (find-empty parsed (length inp))
          updated-galaxies (update-galaxies parsed emptylines emptycol 1)
          updated-galaxies2 (update-galaxies parsed emptylines emptycol 999999)]
    (print (sum (get-all-distances updated-galaxies)))
    (print (sum (get-all-distances updated-galaxies2)))
    ))