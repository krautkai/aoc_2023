(defn- then* [comparators]
  (def len (length comparators))
  (case len
    0 cmp
    1 (first comparators)
    (fn [a b]
      (var result 0)
      (for i 0 len
        (def comparator (in comparators i))
        (set result (comparator a b))
        (when (not= 0 result)
          (break)))
      result)))

# given a list of comparators, return a comparator that tries each of them
# in order until one returnes non-zero
(defn then [& comparators] (then* comparators))

(defn by [f &opt comparator]
  (default comparator cmp)
  (def f (if (keyword? f) (fn [x] (in x f)) f))
  (fn [a b] (comparator (f a) (f b))))

# descending
(defn desc [comparator]
  (fn [a b] (* -1 (comparator a b))))

(defn desc [& args]
  (case (length args)
    0 (desc cmp)
    1 (let [comparator (args 0)] (fn [a b] (* -1 (comparator a b))))
    2 (* -1 (cmp (args 0) (args 1)))
    (error "too many arguments to (desc))")))

# lifts a comparator to a comparator that acts over iterables.
# shorter iterables compare before longer iterables if the short
# iterable is a prefix of the longer iterable
(defn each [comparator] (fn [as bs]
  (var result 0)
  (var a-iterator (next as))
  (var b-iterator (next bs))
  (while (and (not= nil a-iterator) (not= nil b-iterator))
    (def a (get as a-iterator))
    (def b (get bs b-iterator))
    (set result (comparator a b))
    (set a-iterator (next as a-iterator))
    (set b-iterator (next bs b-iterator))
    (when (not= 0 result)
      (break)))
  (if (= result 0)
    (if (= a-iterator nil)
      (if (= b-iterator nil) 0 -1)
      1)
    result)))


(def- core/sort sort)
(def- core/sorted sorted)

(defn- before? [comparator] (fn [a b] (neg? (comparator a b))))

(defn sort [arr & comparators]
  (core/sort arr (before? (then* comparators))))

(defn sorted [ind & comparators]
  (core/sorted ind (before? (then* comparators))))