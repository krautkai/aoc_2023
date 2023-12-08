(def parser
     ~{:navigation (/ (* (<- (repeat 3 :a)) " = (" (<-(repeat 3 :a)) ", " (<-(repeat 3 :a)) ")" :s*), array)
       :instruction  (* (<-(some :a)) :s*)
       :main (/ (* :instruction (some :navigation)),|{:instruction $0 :maps $&})})

(defn to-tables [t]
    (def tab @{})
    (each mp t 
    (put tab (mp 0) {:left (mp 1) :right (mp 2)}))
    tab)

(defn to-zzz [xs pattern] #L = 76, R = 82 :)
    (var position "AAA")
    (def len (length pattern))
    (var i 0)
    (var count 0)
    (while true
        (cond
            (= 76 (pattern i)) (set position ((xs position) :left))
            (= 82 (pattern i)) (set position ((xs position) :right)))
            (set i (% (++ i) len))
            (++ count)
            (if (= position "ZZZ") (break)))
        count)

(defn main [&]
(let [str  (slurp "inputs/day8")
      [parsed] (peg/match parser str)
      maps (to-tables (parsed :maps))]
#(pp str)
(print (to-zzz maps (parsed :instruction)))))