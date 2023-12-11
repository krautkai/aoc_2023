(def parser
    '{:other (drop :a)
      :main  (some (choice (number :d) :other))})

(def parser2
    '{:digit (choice
        (/ (if "one" 2 ) 1)
        (/ (if "two" 2 ) 2)
        (/ (if "three" 3 ) 3)
        (/ (if "four" 4 ) 4)
        (/ (if "five" 3 ) 5)
        (/ (if "six" 3 ) 6)
        (/ (if "seven" 4 ) 7)
        (/ (if "eight" 3 ) 8)
        (/ (if "nine" 3 ) 9)
        (number :d))
      :other (drop :a)
      :main  (some (choice :digit :other))})

(defn value [line p]
    (let [numbers (peg/match p line)]
    (parse (string (first numbers)
                   (last numbers)))))

(defn main [&]
    (let [input (string/split "\n" (string/trim(slurp "inputs/day1")))
        line-numbers1 (map |(value $ parser) input)
        line-numbers2 (map |(value $ parser2) input)]
    (print (sum line-numbers1))
    (print (sum line-numbers2))))