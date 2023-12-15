(defn part [l col nb endcol]
    {:line (- l 1)
     :col (- col 1)
     :number nb
     :endcol (- endcol 1)})

(def parser
     ~{:number (/ (* (line) (column) (number :d+) (column)) , part)
       :main (any (* (any :D) :number))})

(defn gear [l c]
    {:l (- l 1)
     :c (- c 1)})

(def gear-parser
    ~{:gear (/ (* (line) (column) "*") ,gear)
      :main (any (* (any (if-not "*" 1)) :gear))})

(def symb-parser
    ~{:notsymb (+ :d ".")
      :symb (if-not :notsymb 1)
      :main  (* (any :notsymb) :symb)})

(defn get-neighbors [x start end]
  (defn neigh [i] (max 0 (min i (length x))))
  (slice x (neigh start) (neigh (+ end 1))))
  
(defn map-valids [valids]
  (tabseq [valid :in valids
           c :range [(valid :col) (valid :endcol)]] 
    {:y c :x (valid :line)} valid)) 
  
(defn has-symbols [{:line line :col column :number number :endcol endcol} input]
    (def lines (get-neighbors input (- line 1) (+ line 1)))
    (def cols (map |(get-neighbors $ (- column 1) endcol) lines))
    (some |(peg/match symb-parser $) cols))

(defn has-values [{:l l :c c} valids]
    (def neighbors @{})
    (seq [x :range-to [(- l 1) (+ l 1)]
        y :range-to [(- c 1) (+ c 1)]]
           (def part (valids {:x x :y y}))
    (if part (put neighbors part (part :number))))
    (if (= 2 (length neighbors))
        (product neighbors)
        0))

(defn main [&]
    (let [file "inputs/day3"
          str (slurp file)
          input (string/split "\n" (slurp file))
          parsed (peg/match parser str)
          valids (filter |(has-symbols $ input) parsed)
          res1 (sum (map |( $ :number)valids))
          gears (peg/match gear-parser str)
          valids-map (map-valids valids)
          res2 (sum (map |(has-values $ valids-map) gears))]
          (print "Day03 : " res1 " " res2)))
