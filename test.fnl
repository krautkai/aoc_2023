(fn keys [t]
  (var keys-list [])
  (each [k _ (pairs t)]
    (table.insert keys-list k ))
    keys-list)

(fn mvalues [t]
  (var val-list [])
  (each [_ v (pairs t)]
    (table.insert val-list v ))
    val-list)


(fn merge-tables [table1 table2]
  (var combined [])
    (each [k v (pairs table1)]
      (table.insert combined k v))
    (each [k v (pairs table2)]
      (table.insert combined k v))
  combined)

(fn acopy [t]
  (var r {})
  (each [_ v (ipairs t)]
    (table.insert r v))
  r)

(fn sort-table-by-value [t]
  (var sorted [])
  (let [ks (keys t)]
    (var ks-sorted (acopy ks))
    (table.sort ks-sorted)
    (each [k v (pairs ks-sorted)]
      (table.insert sorted (. t v)))
    sorted))

(fn parse-digits [line]
  (let [digits { "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 }
       digits-re (merge-tables (keys digits) (mvalues digits))
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
    (let [matches-sorted (sort-table-by-value matches)]
    (each [_ v (pairs matches-sorted)]
      (print v))
    matches-sorted)))

(parse-digits "two1nine")
(parse-digits "zoneight234")
(parse-digits "xtwone3four")
(parse-digits "eightwothree")
(parse-digits "abcone2threexyz")

