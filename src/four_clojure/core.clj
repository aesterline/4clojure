(ns four-clojure.core)

(defn num-seq [& args]
  (map #(Character/getNumericValue %) (str (apply * args))))

(defn least-common-multiple [& args]
  (let [smallest (apply min args)
        natural-numbers (iterate inc 1)
        multiple? (fn [x] (every? #(zero? (rem x %)) args))
        possible-multiples (map (partial * smallest) natural-numbers)]
    (first (filter multiple? possible-multiples))))

(defn greatest-common-divisor [& args]
  (let [smallest (apply min args)
        divisor? (fn [x] (every? #(zero? (rem % x)) args))
        possible-divisors (range 1 (inc smallest))]
    (last (filter divisor? possible-divisors))))

(defn my-iterate [f i]
  (cons i (lazy-seq (my-iterate f (f i)))))