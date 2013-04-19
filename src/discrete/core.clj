(ns discrete.core)

;; from http://programming-pages.com/2012/01/16/recursion-in-clojure/
(defn power
  ([x y] (power x y 1))
  ([x y current]
  (if (= y 0)
    current
    (if (> y 0)
      (recur x (- y 1) (*' x current))
      (recur x (+ y 1) (/ current x))))))

; a^b mod n
(defn seed-planter [a current bit]
  (let [doubled (*' current current)]
    (if (= bit 1)
      (*' doubled a)
      doubled)))

; a^b through seed planting
(defn power-sp [a b]
  (let [bits-of-b (map #(Character/digit %1 2) (Long/toString b 2))]
    (loop [current 1
           bits bits-of-b]
      (if (= (count bits) 0)
        current
        (recur (seed-planter a current (first bits)) (rest bits))))))
      ;     seed-planter (fn [current bit]
      ;                     (if (= bit 1)
      ;                       (*' current current a)
      ;                       (*' current current)))]
      ; (println bits-of-b)
      ; (map (partial seed-planter a) bits-of-b)))

(defn dot [v1 v2]
  (reduce + (map * v1 v2)))

;; using Euclid's algorithm
(defn gcf [a b] 
  (if (= b 0) a 
      (gcf b (mod a b))))

(defn power-mod
  ([a b n] (power-mod a b 1 n))
  ([a b current n]
    (if (= b 0)
      current
      (recur a (dec b) (mod (* a current) n) n))))

;; from http://clojure.roboloco.net/?tag=proper-divisors
(defn proper-divisors [x]
  (filter #(zero? (mod x %1)) (range 1 (+ 1 (/ x 2)))))

(defn aliquot-sum [x]
  (reduce + (proper-divisors x)))

(defn fact [x] (reduce *' (range 1 (inc x))))

(defn factorials []
  (letfn [(factorial-seq [n1 n2]
            (lazy-seq 
              (cons fact (factorial-seq (inc n1) (*' (inc n1) n2)))))]
  (factorial-seq 1 1)))

(defn sum-of-digits [x]
  (apply + (map #(Character/digit %1 10) (str x))))


(defn isbn-10 [x]
  (let [coefficients (reverse (range 1 11))
        isbn (apply str (filter #(Character/isDigit %1) (str x)))
        digits (map #(Character/digit %1 10) isbn)
        dot-product (dot coefficients digits)]
    (= (mod dot-product 11) 0)))

(defn isbn-13 [x]
  (let [coefficients (flatten (repeat [1 3]))
        isbn (apply str (filter #(Character/isDigit %1) (str x)))
        digits (map #(Character/digit %1 10) isbn)
        dot-product (dot coefficients (take 12 digits))
        modulus (mod dot-product 10)
        check-digit (last digits)]
    (= (- 10 check-digit) modulus)))


