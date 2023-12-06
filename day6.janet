(def parser
    ~{:distances (/(* "Distance:" :s* (some (* (number :d+) :s*))), array)
      :times (/(* "Time:" :s* (some (* (number :d+) :s*))), array)
      :main (/ (* :times :distances), |{:times $0 :distances $1})})

(def parser2
    ~{:distances (%(* "Distance:" :s* (some (* (number :d+) :s*))), :distance)
    :times (%(* "Time:" :s* (some (* (number :d+) :s*))), :time)
    :main  (* :times :distances)})

(defn possibilites [[time distance]]
(var pos 0)
(loop [i :range-to [0 time]]
    (def v i)
    (def d (* v (- time i)))
    (if (> d distance) (++ pos)))
    pos
)

(defn main [&]
(let [str (slurp "inputs/day6")
    [{:times times :distances distances}] (peg/match parser str)
    zipped (map tuple times distances)
    input2 (map scan-number (peg/match parser2 str))]
    (print (product (map possibilites zipped)))
    (print (possibilites [(input2 0) (input2 1)]))
))