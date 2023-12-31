(defn print-table [t]
  (each row t
    (print row)))

(defn tonum [n]
  (int/to-number (int/u64 n)))

(defn print-dict [t]
  (loop [[letter word] :pairs t]
  (print letter " : " word)))

  (defn split-array [arr n]
    (var biga @[])
    (var nb-arrays (/ (length arr) n))
    (var total 0)
    (var i 0)
    (while (< i nb-arrays)
    (for i 0 nb-arrays (+= total i))
    (array/push biga (array/slice arr (* i n) (+ (* i n) n ) ))
    (++ i))
    biga)
  
(defn intersection [t1 t2]
    (var inter @[])
    (each el t1
        (if (has-value? t2 el) (array/push inter el)))
        inter)

(defn same-elems [xs]
    (if (empty? xs) true
    (let [fst (first xs)]
        (all |(= fst $)xs)
    )))

(defn all-zeros [xs]
    (all zero? xs))