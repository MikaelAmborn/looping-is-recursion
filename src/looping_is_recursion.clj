(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         b-seq a-seq]
    (cond
      (empty? b-seq) nil
      (pred (first b-seq)) index
      :else (recur (inc index) (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         num-elements 0
         b-seq a-seq]
    (if (empty? b-seq)
      (/ sum num-elements)
      (recur (+ sum (first b-seq)) (inc num-elements) (rest b-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odd-ones #{}
           b-seq a-seq]
      (if (empty? b-seq)
        odd-ones
        (recur
          (toggle odd-ones (first b-seq))
          (rest b-seq))))))

(defn fast-fibo [n]
  (loop [fn-1 0 f-n 0 curr 0]
    (cond
      (= (curr n)) f-n
      (= (curr 0)) 0
      :else (recur fn-1 (+ fn-1 f-n) (inc curr)))))


(defn cut-at-repetition [a-seq]
  (loop [visited #{}
         b-seq a-seq
         result []]
    (cond
      (empty? b-seq) result
      (contains? visited (first b-seq)) result
      :else (recur (conj visited (first b-seq))
                   (rest b-seq)
                   (conj result (first b-seq))))))

