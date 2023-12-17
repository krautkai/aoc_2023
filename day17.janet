(import ./utils/heapq :as heapq)

(def parser 
    ~{:grid (/(* (line)(column) (number :d)), |{:coor [(- $1 1) (- $0 1)] :val $2})
      :main (some(choice "." "\n" :grid))})

(def turns [[1 0] [0 1] [-1 0] [0 -1]])

(defn get-table [t]
    (def tab @{})
    (each el t
        (put tab (el :coor) (el :val)))
        tab)

(defn minimum-heat [start end minsteps maxsteps grid]
    (var mini-heat 0)
    (var queue @[[0 0 0 0 0]])
    (def visited @[])
    (while (not (empty? queue))
    #(def [heat x y px py] (queue 0))
    #(array/remove queue 0)
    (def [heat x y px py] (array/pop queue))
    (set queue (heapq/heapify queue))
    (if (= end [x y]) (do (set mini-heat heat)(break)))
    (if (not(has-value? visited [x y px py]))
    (do
        (array/push visited [x y px py])
        (each el turns
            (if (and(not (= el [px py]))(not (= el [(- px) (- py)])))
            (do
                (var a x)
                (var b y)
                (var h heat)
                (loop [i :range [1 (+ maxsteps 1)]]
                    (+= a (el 0))
                    (+= b (el 1))
                    (if (has-key? grid [a b])
                    (do 
                        (+= h (grid [a b]))
                        (if (>= i minsteps)
                        (do
                            (array/push queue [h a b (el 0) (el 1)])
                            (set queue (heapq/heapify queue))
                            )))))))))))
                        mini-heat)
    

(defn main [&]
    (let [input (string/trim (slurp "inputs/day17"))
          grid (get-table(peg/match parser input))
          len (- (length (string/split "\n" input)) 1)]
    (print (minimum-heat [0 0] [len len] 1 3 grid))
    ))