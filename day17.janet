(import ./utils/heap :as heap)
(import ./utils/cmp :as cmp2)

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
    (def queue (heap/new (cmp2/by :heat)))
    (heap/push queue {:heat 0 :x 0 :y 0 :px 0 :py 0})
    (def visited @{})
    (while (not (= 0 (heap/length queue)))
    (def {:heat heat :x x :y y :px px :py py} (heap/pop-min queue))
    (if (= end [x y]) (do (set mini-heat heat)(break)))
    (if (not(has-key? visited [x y px py]))
    (do
        (put visited [x y px py] true)
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
                            (heap/push queue {:heat h :x a :y b :px (el 0) :py (el 1)})
                            )))))))))))
                        mini-heat)
    

(defn main [&]
    (let [input (string/trim (slurp "inputs/day17"))
          grid (get-table(peg/match parser input))
          len (- (length (string/split "\n" input)) 1)]
    (print (minimum-heat [0 0] [len len] 1 3 grid))
    (print (minimum-heat [0 0] [len len] 4 10 grid))
    ))