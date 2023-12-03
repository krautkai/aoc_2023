
(defn print-table [t]
  (each row t
    (print row)))

(defn tonum [n]
  (int/to-number (int/u64 n)))

(defn print-dict [t]
  (loop [[letter word] :pairs t]
  (print letter " : " word)))

(defn read [path]
  (string/split "\n" (string/trim (slurp path))))

(def nb-cubes {:red 12
               :green 13
               :blue 14})

(defn string-keys [str]
  (case str
    "blue" :blue
    "red" :red
    "green" :green))

(defn tab-to-struct [t]
  (var dict @{})
  (let [el (string/split ", " t)]
    (each round el
      (let [[cnt color] (string/split " " round)]
      (put dict (string-keys color) (int/to-number (int/u64 cnt))))))
      dict)


(defn parse-game [game-data]
  (let [parts (string/split ": " game-data)
        game-num (tonum((string/split " " (string/trim (parts 0))) 1))
        game-info (string/split "; " (parts 1))
        rounds (map tab-to-struct game-info)]
    {:game game-num :rounds rounds}))

(defn verify [game]
  (var valid? true)
  (let [rounds (game :rounds)]
    (each tirage rounds
        (loop [k :keys tirage]
          (let [val (tirage k) mx (nb-cubes k)]
          (if (>  val mx)
          ((set valid? false) (break)))))))
  (= valid? true))

(defn sum-tab [t]
  (var total 0)
  (each x t
  (set total (+ total (x :game))))
  total)

(defn main [&]
  (let [inp (read "inputs/day2")
        parsed (map parse-game inp)
        filtered (filter verify parsed)]
    (print (sum-tab filtered))))
