(import ./utils/utils :as utl)

(def parser
    ~{:seeds (/ (* "seeds: " (some (* (number :d+) :s*))), array)
      :range (/ (* (number :d+) " " (number :d+) " " (number :d+) "\n") ,|{:dest $0 :src $1 :len $2})
      :map (/ (* (some :S)  " map:" :s* (some :range) :s*), array)
      :main (/ (* :seeds (some :map)), |{:seeds $0 :maps $&})})

(defn main [&]
    (let [str (slurp "inputs/test")
          test (peg/match parser str)]
        (pp test)))