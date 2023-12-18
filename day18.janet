(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])

(def parser 
    ~{:order (/ (* :dir " " (number :d+) " (#" 6 ")"), |[$0 $1])
      :dir (/ '1 ,{"R" right "D" down "L" left "U" up})
      :main (some(* :order (? "\n" )))})
      
(def parser2
    ~{:order (/ (* 1 " "  :d+ " (#" (number 5 16) :dir ")"), |[$1 $0])
      :dir (/ '1 ,{"0" right "1" down "2" left "3" up})
      :main (some(* :order (? "\n" )))})

(defn shoelace [xs] # shoelace formula
    (* 0.5 (sum 
    (seq [i :range [0  (dec(length xs))]]
        (def [x1 y1] (xs i))
        (def [x2 y2] (xs (inc i)))
        (* (+ y1 y2) (- x1 x2))))))

(defn calcul-area [xs]
    (var area 0)
    (var coor @[@[0 0]])
    (var peri 0)
    (each [dir dis] xs
        (def previous (array/peek coor))
        (def current @[(+ (previous 0) (* dis (dir 0))) (+ (previous 1) (* dis (dir 1)))])
        (array/push coor current)
        (+= peri dis))
    (+ peri (- (shoelace coor) (/ peri 2) (- 1)))) # pick's theorem A = interiors + boundaries / 2 - 1

(defn main [&]
    (let [input (slurp "inputs/day18")
          parsed (peg/match parser input)
          parsed2 (peg/match parser2 input)]
    (print (calcul-area parsed))
    (print (calcul-area parsed2))))