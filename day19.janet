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


(defn main [&]
    (let [[workflows ratings] (string/split "\n\n" (slurp "inputs/day19"))
           wf (string/split "\n" workflows)
           parsed (reduce2 merge (mapcat |(peg/match wf-parser $) wf ))
           [rats] (peg/match rat-parser ratings)]
        (print (sum (map |(process $ "in" parsed) rats)))))