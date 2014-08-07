(ns chap1)

(defn f123
  [n]
  (if (< n 3) n
    (apply + (map * (range 1 4) (map #(f123 (- %1 %2)) (repeat n) (range 1 4))))))
    
(defn f123-iter 
  [n n3 n2 n1]
  (if (< n 3) (cond (= n 0) n3 (= n 1) n2 (= n 2) n1)
    (f123-iter (- n 1) n2 n1 (apply + (map * (range 1 4) (list n1 n2 n3))))))

(defn f123-it
  [n]
  (f123-iter n 0 1 2))
