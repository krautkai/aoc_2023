(defn transpose-string [st]
    (let [to-array (string/split "\n" st)
          test (map tuple ;to-array)
          test2 (map |(map string/from-bytes $) test)
          test3 (map string/join test2)
          test4 (string/join test3 "\n")]
    test4))

(defn main [&]
    (let [input (string/split "\n\n" (string/trim (slurp "inputs/test13")))]
    (pp input)
    (pp (map transpose-string input))))