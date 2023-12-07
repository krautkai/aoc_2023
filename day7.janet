(def vals {"2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "T" 10 "J" 11 "Q" 12 "K" 13 "A" 14})

(defn analyze-hand [hand]
    (def content (frequencies hand))
    (var hd 0)
    (cond
        (has-value? content 5) (set hd 6)
        (has-value? content 4) (set hd 5)
        (and (has-value? content 3) (has-value? content 2)) (set hd 4)
        (has-value? content 3) (set hd 3)
        (= 2 (count (fn[x] (= x 2)) (values content))) (set hd 2)
        (has-value? content 2) (set hd 1)
        )
    hd)

(defn to-table [hand]
    (var arr @[])
    (each c hand
        (array/push arr (string/from-bytes c)))
        arr)

(defn mass [xs]
    (var res 0)
    (def rev (reverse xs))
    (def len (length xs))
    (var i 0)
    (while (< i len)
        (set res (+ res (* (rev i) (math/pow 20 i)))) (++ i))
    res)

(defn hands [hand bid]
    @{:hand hand
      :bid (scan-number bid)
      :type (analyze-hand hand)
      :hand-t (mass (map vals (to-table hand)))})

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

(defn main [&]
(let [str (string/split "\n" (string/trim (slurp "inputs/day7")))
      inp (mapcat |(peg/match parser $) str)
      sted (sort inp custom-comparator)
      bids (map (fn [x] (get x :bid)) sted)]
    (print (product-key bids))))