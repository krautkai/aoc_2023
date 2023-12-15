(defn pars [x]
    (def [springs groups] x)
    @{:sp springs :gp (map scan-number (string/split "," groups))})

(defn cnt [val &opt h]
    (def {:sp sp :gp gp} val)
    (default h 0)
    (if (empty? sp)(break 0))
    (var n 0)
    (if (has-value? [35 63] (sp 0)) 
        (+= n (cnt {:sp (string/slice sp 1) :gp gp } (+ 1 h ))))
    (if (and (has-value? [46 63] (sp 0)) (or (and (not(empty? sp)) (= (sp 0) h)) (zero? h)))
        (+= n (cnt {:sp (string/slice sp 1) :gp (if (zero? h) gp (string/slice gp 1)) })))
     n   
        )

(defn main [&]
    (let [input (map |(string/split " " $) (string/split "\n" (string/trim (slurp "inputs/test12"))))
          parsed (map pars input)]
    #(pp parsed)
    (pp(map cnt parsed))))