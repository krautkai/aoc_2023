(local tbl (require :utils/tbl))

(fn lines-from-file [name]
  (let [fin (io.open name "r")]
    (if
      fin (icollect [line (fin:lines)] line)
      (assert false (.. "?f " name)))))

(fn read [x f]
  (tbl.map x f ))

(fn firstlast [t]
  (+ (* 10 (tbl.head t)) (tbl.last t))
)

(fn parse-digits [line]
  (let [digits { "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 }
       digits-re (tbl.merge-tables (tbl.keys digits) (tbl.mvalues digits))
        matches {}]
    (each [k v (pairs digits-re)]
      (var done? false)
      (var pos 1)
      (while (not done?)
        (let [result (string.find line v pos)]
          (if result
            (do
              (let [(start end) (string.find line v pos) digit (tonumber (string.sub line start end))]
                (if digit
                  (tset matches start digit)
                  (tset matches start (. digits v)))
                (set pos (+ start 1))))
            (set done? true)))))
    (let [matches-sorted (tbl.sort-table-by-value matches)]
    matches-sorted)))

(fn calcul [t]
  (-> t
    (tbl.map firstlast)
    (tbl.sum)))

(fn main []
 (let [input (lines-from-file "inputs/day1")]
    (let [p (read input #(icollect [v ($1:gmatch "%d")] v)) r1 (calcul p) p2 (read input parse-digits) r2 (calcul p2)]
    (print r1 r2)
    )))

(main)