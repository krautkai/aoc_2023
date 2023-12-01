(fn parse-digits [line]
  (let [digits { "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 }
        matches []]
    (each [k v (pairs digits)]
      (var done? false)
      (var pos 1)
      (while (not done?)
      (print k pos line)
        (let [result (string.find line k pos)]
          (if result
            (do
              (let [[start end] (string.find line k pos) digit (tonumber (string.sub line start end))]
                (if digit
                  (table.insert matches digit)
                  (table.insert matches (. digits k)))
                (set pos (+ start 1))))
            (set done? true)))))
    matches))

(parse-digits "eightwothree")