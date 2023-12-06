(import ./utils/utils :as utl)

(def parser
    ~{:seeds (/ (* "seeds: " (some (* (number :d+) :s*))), array)
      :range (/ (* (number :d+) " " (number :d+) " " (number :d+) "\n") ,|{:dest $0 :src $1 :len $2})
      :maps (/ (* (some :S)  " map:" :s* (some :range) :s*), array)
      :main (/ (* :seeds (some :maps)), |{:seeds $0 :maps $&})})

(defn get-new-seed [maps seed]
  (var sd seed)
  (loop [[k v] :pairs maps]
    (if (<= (v :src) sd (- (+ (v :src) (v :len)) 1) )
    ((set sd (+(v :dest) (- sd (v :src)) )) (break))))
    sd)

(defn proceed [init-seed maps]
  (var seed init-seed)
  (each m maps
    (set seed (get-new-seed m seed)))
    seed)

(defn main [&]
    (let [str (slurp "inputs/day5")
          [input] (peg/match parser str)
          maps (input :maps)
          seeds (input :seeds)
          final-seeds (map |(proceed $ maps) seeds)]
        (pp final-seeds)
        (print (apply min final-seeds))))