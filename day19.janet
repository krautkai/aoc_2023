(def dict {">" > "<" <})

(def wf-parser
    ~{:comparator (+ ">" "<")
      :comparison (/ (* (<- :a) (<- :comparator) (number :d+) ":" (<- :a+)), |{:sym $0 :comp (dict $1) :val $2 :dir $3})
      :direction (<- :a+)
      :condition (+ :comparison :direction)
      :conditions  (* :condition (? ","))
      :main (/(* (<-(some :a)) "{" (some :conditions) "}")  , |{$0 $&})})

(def rat-parser
    ~{  :workflow (/(* "{x=" (number :d+)",m="(number :d+)",a="(number :d+)",s="(number :d+)"}"), |{"x" $0 "m" $1 "a" $2 "s" $3})
        :main (/(some(* :workflow (? "\n"))), array)})

(defn process [xs step workflow]
    (var res 0)
    (cond
        (= step "A")(set res (sum (values xs)))
        (= step "R") (set res 0)
    (do
    (def tests (workflow step))
    (each test tests
            (if(= test "A")(do (set res (sum (values xs)))(break)))
            (if(= test "R")(do (set res 0) (break)))
            (if(= (type test) :string) (do (set res (process xs test workflow)) (break)))
            (if(= true ((test :comp) (xs  (test :sym)) (test :val)))(do (set res (process xs (test :dir) workflow))(break))))))
   res)

(def possibilities @[])

(defn part2 [step coordinate workflow]
    (def new-coor @{})
    (merge-into new-coor coordinate)
    (cond 
        (= step "A") (array/push possibilities new-coor)
        (= step "R") (break)
        (do
            (def tests (workflow step))
            (each test tests
                (if(= test "A")(do (array/push possibilities new-coor)(break)))
                (if(= test "R")(break))
                (if(= (type test) :string) (do (part2 test new-coor workflow) (break)))
                (if (= < (test :comp)) (do(part2 (test :dir) (merge new-coor {(test :sym) [(min (- (test :val) 1) ((new-coor (test :sym)) 0))
                                                                              (min (-(test :val)1) ((new-coor (test :sym)) 1))] }) workflow)
                (merge-into new-coor {(test :sym) [(max (test :val) ((new-coor (test :sym)) 0)) (max (test :val) ((new-coor (test :sym)) 1))] })))

            (if (= > (test :comp)) (do(part2 (test :dir) (merge new-coor {(test :sym) [(max (+ 1(test :val)) ((new-coor (test :sym)) 0))
                                                                              (max (+ 1(test :val)) ((new-coor (test :sym)) 1))] }) workflow)
                (merge-into new-coor {(test :sym) [(min (test :val) ((new-coor (test :sym)) 0))
                                                                              (min (test :val) ((new-coor (test :sym)) 1))] })))))))

(defn calculate [xs]
    (var res 0)
    (each el xs
        (var ssres 1)
        (each e el
        (*= ssres (- (e 1) (e 0) (- 1))))
        (+= res ssres))
        res)


(defn main [&]
    (let [[workflows ratings] (string/split "\n\n" (slurp "inputs/day19"))
           wf (string/split "\n" workflows)
           parsed (reduce2 merge (mapcat |(peg/match wf-parser $) wf ))
           [rats] (peg/match rat-parser ratings)]
        (print (sum (map |(process $ "in" parsed) rats)))
        (part2 "in" {"x" [1 4000] "m" [1 4000] "a" [1 4000] "s" [1 4000]} parsed)
        (print (calculate possibilities))
        ))