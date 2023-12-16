(def parser 
    ~{:elements (+ "|" "-" "/" "\\")
      :grid (/(* (line)(column) (<- :elements)), |{:coor [(- $1 1) (- $0 1)] :val $2})
      :main (some(choice "." "\n" :grid))})

(defn get-table [t]
    (def tab @{})
    (each el t
        (put tab (el :coor) (el :val)))
        tab)

(defn get-next-coor [coor direction dim]
    (def [x y] coor)
    (cond
        (= direction :up)
        (if (>= (- y 1) 0) (break [x (- y 1)]))
        (= direction :down)
        (if (< (+ 1 y) dim) (break [x (+ y 1)]))
        (= direction :left)
        (if (>= (- x 1) 0) (break [(- x 1) y]))
        (= direction :right)
        (if (< (+ 1 x) dim) (break [(+ 1 x) y])))
    nil)

(defn make-move [coor direction elements visited dim]
    (if(nil? coor) (break @{}))
    (def grid @{coor 1})
    (def visited? (visited coor))
    (if (nil? visited?) (put visited coor @[direction])
    (if (has-value? visited? direction)
    (break @{})
    (array/push (visited coor) direction)))
    (def current (elements coor))
    (cond
        (= current "-")(if (or(= direction :right) (= direction :left))
        (merge-into grid (make-move (get-next-coor coor direction dim) direction elements visited dim))
        (merge-into grid (make-move (get-next-coor coor :right dim) :right elements visited dim) (make-move (get-next-coor coor :left dim) :left elements visited dim)))
        (= current "/")
            (cond 
                (= direction :up) (merge-into grid (make-move (get-next-coor coor :right dim) :right elements visited dim))
                (= direction :left) (merge-into grid (make-move (get-next-coor coor :down dim) :down elements visited dim))
                (= direction :down) (merge-into grid (make-move (get-next-coor coor :left dim) :left elements visited dim))
                (= direction :right) (merge-into grid (make-move (get-next-coor coor :up dim) :up elements visited dim)))
        (= current "\\")
            (cond
            (= direction :up) (merge-into grid (make-move (get-next-coor coor :left dim) :left elements visited dim))
            (= direction :left) (merge-into grid (make-move (get-next-coor coor :up dim) :up elements visited dim))
            (= direction :down) (merge-into grid (make-move (get-next-coor coor :right dim) :right elements visited dim))
            (= direction :right) (merge-into grid (make-move (get-next-coor coor :down dim) :down elements visited dim)))
        (= current "|")(if (or(= direction :up) (= direction :down))
        (merge-into grid (make-move (get-next-coor coor direction dim) direction elements visited dim))
        (merge-into grid (make-move (get-next-coor coor :up dim) :up elements visited dim) (make-move (get-next-coor coor :down dim) :down elements visited dim)))
        
        (merge-into grid (make-move (get-next-coor coor direction dim) direction elements visited dim)))
    grid)

(defn part2 [parsed dim]
    (var res 0)
    #top
    (loop [i :range [0 dim]]
        (def test (length (make-move [i 0] :down parsed @{} dim)))
        (if (< res test) (set res test)))
    #left
    (loop [i :range [0 dim]]
        (def test (length (make-move [0 i] :right parsed @{} dim)))
        (if (< res test) (set res test)))
    #bottom
    (loop [i :range [0 dim]]
        (def test (length (make-move [i (- dim 1)] :up parsed @{} dim)))
        (if (< res test) (set res test)))
    #right
    (loop [i :range [0 dim]]
        (def test (length (make-move [(- dim 1) i] :left parsed @{} dim)))
        (if (< res test) (set res test)))
    res)

(defn main [&]
    (let [input (string/trim (slurp "inputs/day16"))
          input-split (string/split "\n" input)
          dim (length input-split)
          parsed (get-table(peg/match parser input))
          res (make-move [0 0] :right parsed @{} dim)]
    (pp (length res))
    (print (part2 parsed dim))))