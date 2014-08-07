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
