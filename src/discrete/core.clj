(ns discrete.core)


(def pos-int (rest (range)))

;; from http://programming-pages.com/2012/01/16/recursion-in-clojure/
(defn power
  ([x y] (power x y 1))
  ([x y current]
  (if (= y 0)
    current
    (if (> y 0)
      (recur x (- y 1) (*' x current))
      (recur x (+ y 1) (/ current x))))))

;; from http://clojure.roboloco.net/?tag=proper-divisors
(defn proper-divisors [x]
  (filter #(zero? (mod x %1)) (range 1 (+ 1 (/ x 2)))))

(defn aliquot-sum [x]
  (reduce + (proper-divisors x)))

(defn dot [v1 v2]
  (reduce + (map * v1 v2)))


; find greatest common divisor using Euclid's algorithm
(defn gcd [a b]
  (if (= b 0) a 
      (gcd b (mod a b))))

(defn relatively-prime? [a b]
  (= (gcd a b) 1))


(defn fact [x] (reduce *' (range 1 (inc x))))

(defn factorials []
  (letfn [(factorial-seq [n1 n2]
            (lazy-seq 
              (cons fact (factorial-seq (inc n1) (*' (inc n1) n2)))))]
  (factorial-seq 1 1)))

(defn sum-of-digits [x]
  (apply + (map #(Character/digit %1 10) (str x))))

(defn seed-planter [a current bit]
  (let [squared (*' current current)]
    (if (= bit 1)
      (*' squared a)
      squared)))

(defn to-bits [b] 
  (map #(Character/digit %1 2) (.toString (biginteger b) 2)))

; a^b through seed planting
(defn power-sp [a b]
  (let [bits-of-b (to-bits b)]
    (loop [current 1
           bits bits-of-b]
      (if (= (count bits) 0)
        current
        (recur (seed-planter a current (first bits)) (rest bits))))))

; a^b (mod n) through seed planting
(defn power-mod [a b n]
  (let [bits-of-b (to-bits b)]
    (loop [current 1
           bits bits-of-b]
      (if (= (count bits) 0)
        current
        (recur (mod (seed-planter a current (first bits)) n) (rest bits))))))

; horrible horrible prime test
(defn accurate-prime? [x] (= (count (proper-divisors x)) 1))

(defn fermat-prime? [n]
  (let [a 9876]
    (= (power-mod a n n) a)))

; If Fermat test succeeds, check the slow way
(defn checked-fermat-prime? [n]
  (if-not 
    (fermat-prime? n) false
    (accurate-prime? n)))

(def carmichael-numbers
  (letfn [(carmichael? [n]
           (and
            (fermat-prime? n)
            (not (accurate-prime? n))))]
  (lazy-seq (filter carmichael? pos-int))))

; The number of numbers in {1, 2, .., n} that are relatively prime to n
(defn phi [n]
  (loop [current 1 rp-count 0]
    (if (= current n)
      rp-count
      (if (relatively-prime? current n)
        (recur (inc current) (inc rp-count))
        (recur (inc current) rp-count)))))

(defn pki-e [d phi-n f]
  (/ (+ 1 (* phi-n f)) d))

(defn integer-pki-e [d phi-n]
  (loop [f 1]
    (let [e (pki-e d phi-n f)]
    (if (not (ratio? e))
      e
      (recur (inc f))))))

(defn isbn-10 [x]
  (let [coefficients (reverse (range 1 11))
        isbn (apply str (filter #(Character/isDigit %1) (str x)))
        digits (map #(Character/digit %1 10) isbn)
        dot-product (dot coefficients digits)]
    (= (mod dot-product 11) 0)))

(defn isbn-13 [x]
  (let [coefficients (cycle [1 3])
        isbn (apply str (filter #(Character/isDigit %1) (str x)))
        digits (map #(Character/digit %1 10) isbn)
        dot-product (dot coefficients (take 12 digits))
        modulus (mod dot-product 10)
        check-digit (last digits)]
    (= (- 10 check-digit) modulus)))


; A sequence
; Order matters, repetition is allowed
(defn sequence-count [x n]
  (power x n))

; An arrangement
; Order matters, reptition is not allowed
(defn arrangement-count [n k]
  (/ (fact n) (fact (- n k))))

; A combination or subset
; Order does not matter, repetition is not allowed
(defn binomial-coefficient [n k]
  (/ (fact n) (* (fact k) (fact (- n k)))))

; A multisubset
; Order does not matter, repitition is allowed
;(n+k-1, k)
(defn multisubset-count [n k]
  (binomial-coefficient (- (+ n k) 1) k))

; integer partitioning
(defn partitions [k n]
  (cond 
    (= k 0) 0
    (<= n 0) 0
    (= k 1) 1
    (= n 1) 1
    :else (+ (partitions (dec k) (dec n)) (partitions k (- n k)))))

(defn pascal-row [n] 
  (map (partial binomial-coefficient n) (range (inc n))))

(def pascal-triangle (map pascal-row (range)))

; find [x y] where ax+by=1
(defn euclid [a b]
  (loop [x1 1 
         y1 0 
         x2 0 
         y2 1 
         r1 a 
         r2 b]
    (let [q (int (/ r1 r2))
          ri (mod r1 r2)
          xi (- x1 (* q x2))
          yi (- y1 (* q y2))]
      (if (= ri 1) [xi yi]
        (recur x2 y2 xi yi r2 ri)))))


;; From https://gist.github.com/sritchie/1627900
(defn transpose [coll]
  (apply map vector coll))
 
(defn matrix-mult [mat1 mat2]
  (let [row-mult (fn [mat row] 
                   (map (partial dot row) (transpose mat)))]
    (map (partial row-mult mat2) mat1)))

