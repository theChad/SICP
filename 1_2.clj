(ns chap1)

; 1.11
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

; 1.12
; (pascal n) computes *row* n of pascal's triangle, not element n.
; Maybe not exactly what was asked for.
(defn line-right
  [old-line]
  (if (empty? (rest old-line)) '(1)
    (cons (+ (first old-line) (first (rest old-line)))
      (line-right (rest old-line)))))
      
(defn make-line
  [old-line]
  (cons 1 (line-right old-line)))

(defn pascal-recur 
  [n last-line]
  (if (= n 0) last-line
    (recur (dec n) (make-line last-line))))
    
(defn pascal [n]
  (pascal-recur n '(1)))

; 1.16
(defn fast-expt-iter
  [b n a]
  "Helper for fast-iter"
  (if (= 0 n) a
    (if (even? n)
      (recur (*' b b) (/ n 2) a)
      (recur b (- n 1) (* a b)))))

(defn fast-expt
  "Iteratively calculate b^n, using a helper function."
  [b n]
  (fast-expt-iter b n 1))

; 1.17 & 1.18
(defn fast-mult-iter
  [a b prod]
  (cond (= b 0) prod
    (even? b) (recur (* 2 a) (/ b 2) prod)
    :else (recur a (dec b) (+ a prod))))

(defn fast-mult
  [a b]
  (fast-mult-iter a b 0))

; 1.19
; p' = p^2 + q^2
; q' = q^2 + 2*p*q
(defn fib-iter
  [n a b p q]
  (cond (= n 0) b
    (even? n)
      (let [p-prime (+ (* p p) (* q q))
            q-prime (+ (* q q) (* 2 p q))]
        (fib-iter (/ n 2) a b p-prime q-prime))
    :else
      (fib-iter (dec n) 
        (+ (* b q) (* a q) (* a p))
        (+ (* b p) (* a q)) 
        p
        q)))
          
(defn fib
  "Compute Fibonacci sequence in log n"
  [n]
  (fib-iter n 1N 0 0 1N))
