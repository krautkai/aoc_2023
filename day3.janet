(defn part [l col nb endcol]
    {:line (- l 1)
     :col (- col 1)
     :number nb
     :endcol (- endcol 1)})

(def parser
     ~{:number (/ (* (line) (column) (number :d+) (column)) , part)
       :main (any (* (any :D) :number))})

(def symb-parser
    ~{:notsymb (+ :d ".")
      :symb (if-not :notsymb 1)
      :main  (* (any :notsymb) :symb)})

(defn get-neighbors [x start end]
  (defn neigh [i] (max 0 (min i (length x))))
  (slice x (neigh start) (neigh (+ end 1))))
  
(defn has-symbols [{:line line :col column :endcol endcol} input]
    (def lines (get-neighbors input (- line 1) (+ line 1)))
    (def cols (map |(get-neighbors $ (- column 1) endcol) lines))
    (some |(peg/match symb-parser $) cols))

(defn main [&]
    (let [file "inputs/day3"
          str (slurp file)
          input (string/split "\n" (slurp file))
          parsed (peg/match parser str)
          valids(filter |(has-symbols $ input) parsed)
          res1  (sum (map |( $ :number)valids)) ]
          (print res1)))
