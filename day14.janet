(defn rocks [col sym] 
    {:c (- col 1)
      :s sym})

(def parser 
    ~{:r-rocks (/(*(column) (<- "O")), rocks)
      :c-rocks (/(*(column) (<- "#")), rocks)
      :main (* (any (+ :c-rocks :r-rocks ".")))})

(defn do-moves [t]
(def len (length t))
(while true
    (var moves 0)
    (loop [i :range [1 len]]
        (def rounded (filter |(= "O" ($ :s)) (i t)))
        (def new-current (filter |(= "#" ($ :s)) (i t)))
        (array/clear (t i))
        (def occupied (map |($ :c) (t (- i 1))))
        (each r rounded
            (if (not(has-value? occupied (r :c)))
            (do(array/push (t (- i 1)) r) (++ moves))
            (array/push new-current r)))
        (each r new-current
            (array/push (t i) r))
    )
(if (zero? moves) (break)))
t)

(defn part1 [t]
    (var res 0)
    (var i 1)
    (each val (reverse t)
    (+= res (* i (length (filter |(= "O" ($ :s)) val))))
    (++ i)
    )
    res)


(defn main [&]
    (let [input (string/split "\n" (string/trim (slurp "inputs/day14")))
          parsed (map |(peg/match parser $) input)
          after-moves (do-moves parsed)]
    (print "Day14 : " (part1 after-moves))))