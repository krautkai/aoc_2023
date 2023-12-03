(defn print-table [t]
  (each row t
    (print row)))

(defn tonum [n]
  (int/to-number (int/u64 n)))

(defn print-dict [t]
  (loop [[letter word] :pairs t]
  (print letter " : " word)))