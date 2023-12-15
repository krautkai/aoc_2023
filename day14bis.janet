(defn transpose [xs] (map tuple ;xs))

(defn roll [xs]
    )

(defn main [&]
    (let [input (string/split "\n" (string/trim (slurp "inputs/test14")))
          inputb (map string/bytes input)]
    (pp inputb)
    (pp (transpose inputb))))