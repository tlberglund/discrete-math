(ns discrete.core)

(def pos-int (rest (range)))

; (def s (reverse (map #(/ 1 %) (range 1 9))))

; (defn p-wrong [n]
;   (reduce *
;           (map (#(/ %1 %2)
;                 reverse (range 1 (dec n))
;                 (reverse (range 2 n))))))

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

(defn perfect? [x]
  (= x (aliquot-sum x)))

(defn deficient? [x]
  (< x (aliquot-sum x)))

(defn abundant? [x]
  (> x (aliquot-sum x)))

(def perfect-numbers (filter perfect? pos-int))
(def abundant-numbers (filter abundant? pos-int))
(def deficient-numbers (filter deficient? pos-int))


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
  (= (power-mod 2 n n) 2))

; If Fermat test succeeds, check the slow way
(defn checked-fermat-prime? [n]
  (if-not 
      (fermat-prime? n) false
      (accurate-prime? n)))

(def prime-numbers (filter checked-fermat-prime? pos-int))

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


(defn pki-d [n]
  (let [phi-n (phi n)
        d-candidates (fn [] (repeatedly (partial rand-int phi-n)))]
    (first 
     (drop-while 
      #(not (relatively-prime? phi-n %1)) 
      (d-candidates)))))


; (defn pki-d [n]
;   (let [filter-fn (partial relatively-prime? n)
;         starting-int (inc n)
;         d-candidates (drop starting-int pos-int)]
;     (first (filter filter-fn d-candidates))))

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
(defn sequence-count [n k]
  (power n k))

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
;  How many ways to partition an integer n into k pieces
(defn partitions [n k]
  (cond 
    (= k 0) 0
    (< n 0) 0
    (= n 0) 1
    (= k 1) 1
    :else (+ (partitions (dec k) (dec n)) (partitions k (- n k)))))


(defn- inner-stirling [n k j]
  (*
    (power -1 (- k j))
    (binomial-coefficient k j)
    (power j n)))

; Closed-form Stirling number of the second kind
(defn stirling-number-2nd [n k]
  (cond
    (= k 1) 1
    (= n k) 1
    :else
    (/
      (reduce + (map #(inner-stirling n k %1) (range (inc k))))
      (fact k))))


(defn pascal-row [n] 
  (map (partial binomial-coefficient n) (range (inc n))))

(def pascals-triangle (map pascal-row (range)))


; find [x y] where ax+by=1
(defn extended-euclid [a b]
  (loop [x1 1 
         y1 0 
         x2 0 
         y2 1 
         r1 a 
         r2 b]
    (if (= r2 0) [x1 y1]
        (let [q (int (/ r1 r2))
              ri (mod r1 r2)
              xi (- x1 (* q x2))
              yi (- y1 (* q y2))]
          (recur x2 y2 xi yi r2 ri)))))


;; From https://gist.github.com/sritchie/1627900
(defn transpose [coll]
  (apply map vector coll))
 
(defn matrix-mult [mat1 mat2]
  (let [row-mult (fn [mat row] 
                   (map (partial dot row) (transpose mat)))]
    (map (partial row-mult mat2) mat1)))

