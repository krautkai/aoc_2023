(def vals {"2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})
(def vals2 {"J" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "T" 10  "Q" 11 "K" 12 "A" 13})

(defn analyze-hand [jokers fc]
    (def content (frequencies fc))
    (var hd 0)
    (cond
        (has-value? content 5) (set hd 5)
        (has-value? content 4) (set hd (+ 4 jokers))
        (and (has-value? content 3) (has-value? content 2)) (set hd 3.5)
        (has-value? content 3) (set hd (+ 3 jokers))
        (= 2 (count (fn[x] (= x 2)) (values content))) (set hd (+ 2 (* 1.5 jokers)))
        (has-value? content 2) (set hd (+ 1 (+ (cond (not ( = 0 jokers)) ( + 1 jokers) 0))))
        (cond 
            (= 5 jokers) (set hd 5)
            (= 4 jokers ) (set hd 5)
            (= 3 jokers) (set hd 4)
            (= 2 jokers) (set hd 3)
            (= 1 jokers) (set hd 1)
        ))
    hd)

(defn to-table [hand]
    (var arr @[])
    (each c hand
        (array/push arr (string/from-bytes c)))
        arr)

(defn weight [xs]
    (var res 0)
    (def rev (reverse xs))
    (def len (length xs))
    (var i 0)
    (while (< i len)
        (set res (+ res (* (rev i) (math/pow 15 i)))) (++ i))
    res)

(defn hands [hand bid]
    @{:hand hand
      :bid (scan-number bid)
      })

(defn custom-comparator [a b]
  (if (= (a :type) (b :type))
    (< (a :hand-t) (b :hand-t))
    (< (a :type) (b :type))))

(def parser
    ~{:bid (/ (* (number :d+) :s*), string)
      :hand (/ (* (capture :w+) :s*), string)
      :main (/ (* :hand :bid), hands)})

(defn product-key [xs]
    (var i 0)
    (var res 0)
    (def len (length xs))
    (while (< i len)
        (set res (+ res (* (+ i 1) (xs i)))) (++ i))
        res)

(defn add-weight [t val part2?]
    (var n t)
    (def hand (t :hand))
    (var jokers 0)
    (var fc hand)
    (set jokers 0)
    (if part2?
        (do(set jokers (length (string/find-all "J" hand)))
        (set fc (string/replace-all "J" "" hand))))
    (def typ (analyze-hand jokers fc))
    (put n :hand-t (weight (map val (to-table (n :hand)))))
    (put n :type typ)
    n)

(defn main [&]
(let [str (string/split "\n" (string/trim (slurp "inputs/day7")))
      inp (mapcat |(peg/match parser $) str)
      inp1 (mapcat |(add-weight $ vals false) inp)
      sted1 (sort inp1 custom-comparator)
      bids1 (map (fn [x] (get x :bid)) sted1)
      
      inp2 (mapcat |(add-weight $ vals2 true) inp)
      sted2 (sort inp2 custom-comparator)
      bids2 (map (fn [x] (get x :bid)) sted2)]
    (print "Day07 : " (product-key bids1) " " (product-key bids2))))