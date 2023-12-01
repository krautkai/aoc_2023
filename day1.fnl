(local tbl (require :utils/tbl))
(local str (require :utils/str))

(fn lines-from-file [name]
  (let [fin (io.open name "r")]
    (if
      fin (icollect [line (fin:lines)] line)
      (assert false (.. "?f " name)))))

(fn read [x]
  (tbl.map x #(icollect [v ($1:gmatch "%d")] v) ))

(fn firstlast [t]
  (+ (* 10 (tbl.head t)) (tbl.last t))
)

(fn part1 [t]
  (-> t
    (tbl.map firstlast)
    (tbl.sum)))

(fn parse2 [t]

)

(fn main []
 (let [input (lines-from-file "inputs/day1")]
    (let [p (read input) r1 (part1 p)]
    (print r1 (. (. p 1) 1))
      ;(print "Day1: " (tbl.sum r-part1) (tbl.sum r-part2))
    )))

(main)