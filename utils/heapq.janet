(defn siftdown [h startpos p]
    (var heap h)
    (var pos p)
    (def newitem (heap pos))
    (while (> pos startpos)
    (def parentpos (brshift (- pos 1) 1))
    (def parent (heap parentpos))
    (if (< newitem parent)
    (do (set (heap pos) parent)
        (set pos parentpos))
    (break)))
    (set (heap pos) newitem)
    heap)

(defn siftup [h p]
    (var heap h)
    (var pos p)
    (let [endpos (length heap)
          startpos pos
          newitem (heap pos)]
        (var  childpos (+ 1 (* 2 pos)))
        (while (< childpos endpos)
        (def rightpos (+ 1 childpos))
        (if (and (< rightpos endpos) (not (< (heap childpos) (heap rightpos))))
        (set childpos rightpos))
        (set (heap pos) (heap childpos))
        (set pos childpos)
        (set childpos (+ 1 (* 2 pos))))
        (set (heap pos) newitem)
        (set heap (siftdown heap startpos pos)))
        heap)

(defn heapify [xs]
    (var x xs)
    (let [n (length x)
          st (math/floor (/ n 2))]
    (if (= 0 n) (break xs))
    (var i st)
    (while (<= 0 i) 
    (set x (siftup x i))
    (-- i)))
    (reverse x))