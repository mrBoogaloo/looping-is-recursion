(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [acc n]
              (if (zero? n)
                acc
                (recur (* acc base) (dec n))))]
    (pow 1 exp)))

(defn last-element [a-seq]
  (let [asd (fn [a]
              (if (empty? (rest a))
                (first a)
                (recur (rest a))))]
    (asd a-seq)))

(defn seq= [seq1 seq2]
  (let [asd (fn [a b]
              (cond
                (and (empty? a) (empty? b))           true
                (or (and (not (empty? a)) (empty? b))
                    (and (not (empty? b)) (empty? a))
                    (not (= (first a) (first b))))    false
                :else (recur (rest a) (rest b))))]
    (asd seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         a a-seq]
    (if (empty? a)
      nil
      (if (pred (first a))
        index
        (recur (inc index) (rest a))))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [acc 0
           n   0
           a   a-seq]
      (if (empty? a)
        (/ acc n)
        (recur (+ (first a) acc)
               (inc n)
               (rest a))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [b []
         a a-seq]
    (if (empty? a)
      (seq b)
      (recur (toggle (set b) (first a))
             (rest a)))))

(defn fast-fibo [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (loop [i 2
                 num1 0
                 num2 1]
            (if (== i n)
              (+ num1 num2)
              (recur (inc i) num2 (+ num1 num2))))))

(defn cut-at-repetition [a-seq]
  (loop [b []
         a a-seq]
    (if (or (empty? a)
            (contains? (set b) (first a)))
      b
      (recur (conj b (first a)) (rest a)))))
