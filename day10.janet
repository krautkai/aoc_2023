(defn symb [l c symb]
    {:l (- l 1)
      :c (- c 1)
      :symb symb})

(def parser 
    ~{:pipe (+ "|" "-" "L" "J" "7" "F" "S")
      :pipes (/(* (line)(column) (<- :pipe)), symb)
      :main (/(some(choice "." "\n" :pipes)), array)})

(defn get-coor [l c direction dims]
    (var res @[])
    (cond 
        (= direction :west) (if (< 0 c) (array/push res {:l l :c (- c 1)}))
        (= direction :east) (if (< c (dims :width)) (array/push res {:l l :c (+ c 1)}))
        (= direction :north) (if (< 0 l) (array/push res {:l (- l 1) :c c}))
        (= direction :south) (if (< l (dims :length)) (array/push res {:l (+ l 1) :c c}))
        )
    res
    )

#(defn in-maps? [coor maps]
#    (filter |(= $ ) maps)
#    )

(defn get-neighbors [elem s maps dims]
    (pp elem)
    (def [l c] elem)
    (var res @[])
    (def rmap @{})
    (cond
        (= s "|") (array/concat res (get-coor l c :north dims ) (get-coor l c :south dims ))
        (= s "-") (array/concat res (get-coor l c :east dims ) (get-coor l c :west dims ))
        (= s "L") (array/concat res (get-coor l c :north dims ) (get-coor l c :east dims ))
        (= s "J") (array/concat res (get-coor l c :north dims ) (get-coor l c :west dims ))
        (= s "7") (array/concat res (get-coor l c :south dims ) (get-coor l c :west dims ))
        (= s "F") (array/concat res (get-coor l c :south dims ) (get-coor l c :east dims ))
        (= s "S") (array/concat res [{:l 2 :c 1}] [{:l 3 :c 0}]) #example
        )
    (each r res 
        (if (has-key? maps [(r :l) (r :c)] )
            (put rmap [ (r :l) (r :c)] (maps [ (r :l) (r :c)]))
        )
    )
rmap)

(defn create-map [t]
    (def tab @{})
    (each el t
        (put tab [(el :l) (el :c)] (el :symb)))
        tab)

(defn get-all-nodes [pos symb maps dims steps dij visited]
    (var new-dij dij)
    (def cstep (+ 1 steps))
    (pp (new-dij pos))
    (array/push visited pos)
    (def nb (get-neighbors pos symb maps dims))
    #(array/push visited pos)
    (loop [[k v] :pairs nb]
        
    (if
        #(not (has-key? pos new-dij)) (set new-dij cstep)
        #(> (new-dij pos) cstep) (set (new-dij pos) cstep))
        (in visited k) (def newd (+ 1 cstep))
        )

        (def new-w (get-all-nodes k v maps dims cstep new-dij))
        #(loop [[p w]])
          #(array/concat weight [cstep] (if (not (has-value? visited k))(get-all-nodes k v maps dims cstep visited)[])))
    )
(pp new-dij)
    new-dij
    )

(defn main [&]
    (let [input (slurp "inputs/test10")
          inp (string/split "\n" input)
          wid (length (inp 0))
          len (length inp)
          [parsed] (peg/match parser input)
          mapped (create-map parsed)
          start (find-index |(= "S" $) mapped)]
    (pp mapped)
    #(pp (get-neighbors [1 2] "J" mapped @{:length 5 :width 5}))
    (pp (get-all-nodes start "S" mapped @{:length len :width wid} 0 @{} @[]))
    ))