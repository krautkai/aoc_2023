(import ./utils/utils :as utl)

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
      (put dict (string-keys color) (utl/tonum cnt)))))
      dict)


(defn parse-game [game-data]
  (let [parts (string/split ": " game-data)
        game-num (utl/tonum((string/split " " (string/trim (parts 0))) 1))
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

(defn part2 [game]
(def tab-min @{:red 0 :blue 0 :green 0})
(let [rounds (game :rounds)]
    (each tirage rounds
        (loop [k :keys tirage]
          (let [val (tirage k) mx (tab-min k)]
          (if (> val mx)
          (set (tab-min k) val))))))
  tab-min)

(defn sum-tab [t]
  (var total 0)
  (each x t
  (set total (+ total (x :game))))
  total)

(defn sum-mult [t]
  (var total 0)
  (each x t
  (var subtotal 1)
  (loop [[k v] :pairs x]
    (set subtotal (* v subtotal)))
    (set total (+ total subtotal)))
    total)

(defn main [&]
  (let [inp (read "inputs/day2")
        parsed (map parse-game inp)
        filtered (filter verify parsed)
        minimum (map part2 parsed)]
    (print "Day2 : " (sum-tab filtered) " " (sum-mult minimum))))
