(import ./utils/utils :as utl)

(def parser
    ~{:seeds (/ (* "seeds: " (some (* (number :d+) :s*))), array)
      :range (/ (* (number :d+) " " (number :d+) " " (number :d+) "\n") ,|{:dest $0 :src $1 :len $2})
      :maps (/ (* (some :S)  " map:" :s* (some :range) :s*), array)
      :main (/ (* :seeds (some :maps)), |{:seeds $0 :maps $&})})

(defn create-maps [maps]
  (var complete @{})
  (each m maps
    (let [src (m :src)
         dest (m :dest)
         len (m :len)]
        (var mp @{})
        (loop [i :range [0 len]]
          (put complete (+ src i) (+ dest i)))))
          complete)

(defn proceed [init-seed maps]
  (var seed init-seed)
  (each m maps
    (set seed (or (m seed) seed)))
    seed)

(defn main [&]
    (let [str (slurp "inputs/day5")
          [input] (peg/match parser str)
          maps (input :maps)
          seeds (input :seeds)
          analyzed-maps (map create-maps maps)
          final-seeds (map |(proceed $ analyzed-maps) seeds)]
        (print (apply min final-seeds))))