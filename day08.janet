(def parser
     ~{:navigation (/ (* (<- (repeat 3 :w)) " = (" (<-(repeat 3 :w)) ", " (<-(repeat 3 :w)) ")" :s*), array)
       :instruction  (* (<-(some :a)) :s*)
       :main (/ (* :instruction (some :navigation)),|{:instruction $0 :maps $&})})

(defn to-tables [t]
    (def tab @{})
    (each mp t 
    (put tab (mp 0) {:left (mp 1) :right (mp 2)}))
    tab)

(defn to-end [xs pattern start end?] #L = 76, R = 82 :)
    (var position start)
    (def len (length pattern))
    (var i 0)
    (var count 0)
    (while true
        (cond
            (= 76 (pattern i)) (set position ((xs position) :left))
            (= 82 (pattern i)) (set position ((xs position) :right)))
            (set i (% (++ i) len))
            (++ count)
            (if (end? position) (break)))
        count)

(defn inputs2 [x]
    (def int @[])
    (each el x 
        (if (= 65 (el 2)) (array/push int el))) #A = 65
        int)

(defn main [&]
(let [str (slurp "inputs/day8")
      [parsed] (peg/match parser str)
      maps (to-tables (parsed :maps))
      starts (inputs2 (keys maps))
      ends (map |(to-end maps (parsed :instruction) $ (fn [x] (= 90 (x 2)))) starts)] #X = 90
(print "Day08 : " (to-end maps (parsed :instruction) "AAA" (fn [x] (= x "ZZZ"))) " " (reduce math/lcm 1 ends))))