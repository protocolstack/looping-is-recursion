(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= n exp)
                   acc
                   (recur (* base acc) (inc n))))]
    (helper 1 0)))

(defn last-element [a-seq]
  (let [helper (fn [acc s]
                 (if (empty? s)
                   acc
                   (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
                 (if (and (empty? s1) (empty? s2))
                   acc
                   (recur (if (and (first s1) (first s2))
                            (= (first s1) (first s2))
                            false)
                          (rest s1)
                          (rest s2))))]
  (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         ret nil
         p1 pred
         s1 a-seq]
    (if (or (empty? s1) ret)
      ret
      (recur (inc idx)
             (if (p1 (first s1))
               idx
               nil)
             p1
             (rest s1)))))

(defn avg [a-seq]
  (loop [sum-items 0
         num-items 0
         s a-seq]
    (if (empty? s)
      (if (== num-items 0)
        0
        (/ sum-items num-items))
      (recur (+ sum-items (first s)) (inc num-items) (rest s)))))

(defn parity [a-seq]
  (loop [the-set #{}
         the-seq a-seq]
    (if (empty? the-seq)
      the-set
      (recur
       (if (contains? the-set (first the-seq))
         (disj the-set (first the-seq))
         (conj the-set (first the-seq)))
       (rest the-seq)))))

(defn fast-fibo [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (loop [f-n-1 0
             f-n 1
             i 1
             times (- n 1)]
        (if (== i times)
          (+ f-n-1 f-n)
          (recur f-n (+ f-n-1 f-n) (inc i) times))))))

(defn cut-at-repetition [a-seq]
  (loop [ret []
         the-set #{}
         s a-seq]
    (if (empty? s)
      ret
      (recur
       (if (contains? the-set (first s))
         ret
         (conj ret (first s)))
       (conj the-set (first s))
       (if (contains? the-set (first s))
         (seq [])
         (rest s)
       )))))
