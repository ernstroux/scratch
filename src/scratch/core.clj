(ns scratch.core)

(defn sum-until
  [coll x]
  (reduce (fn [[tot acc] a]
            (let [new-tot (+ tot a)]
              (cond
                (< new-tot x) [new-tot (cons a acc)]
                (> new-tot x) (reduced [])
                (= new-tot x) (reduced (cons a acc)))))
          [0 []]
          coll))

(defn comb-sum
  [numbers desired-sum]
  (loop [coll (sort numbers)
         x desired-sum
         acc []]
    (if (or (empty? coll) (> (first coll) x))
      acc
      (recur (rest coll) x (cons (sum-until coll x) acc)))))

(defn arrayManipulation [n queries]
  (apply max (reduce (fn [arr [a b k]]
                       (reduce #(assoc %1 %2 (+ (get %1 %2) k))
                               arr
                               (range (- a 1) b)))
                     (vec (replicate n 0))
                     queries)))

(defn maxDifference [a]
  (loop [i 0
         diff 0
         min-so-far (nth a i)]
    (if (< i (- (count a) 1))
      (let [ai (nth a i)
            new-min (min min-so-far ai)
            new-diff (max diff (- ai new-min))]
        (recur (inc i) new-diff new-min))
      (if (= diff 0)
        -1
        diff))))

(defn mergeArrays [a b]
  (->> (loop [x a
              y b
              merged-list '()]
         (cond
           (and (coll? x) (coll? y)) (let [fx (first x)
                                           fy (first y)]
                                       (if (<= fx fy)
                                         (recur (next x) y (conj merged-list fx))
                                         (recur x (next y) (conj merged-list fy))))
           (coll? y) (apply conj merged-list y)
           (coll? x) (apply conj merged-list x)))
       (reverse)))

(defn canReach [x1 y1 x2 y2]
  (if (cond
        (and (= x1 x2) (= y1 y2)) true
        (or (> x1 x2) (> y1 y2)) false
        :else (or (canReach (+ x1 y1) y1 x2 y2) (canReach x1 (+ x1 y1) x2 y2))
        )
    "Yes"
    "No"))

(defn minimumSwaps [arr n]
  (loop [i 0
         s 0
         a arr]
    (if (= i (- n 1))
      s
      (let [ai (nth a i)]
        (if (= (nth a i) (+ i 1))
          (recur (inc i) s a)
          (let [j (- ai 1)
                aj (nth a j)
                b (->
                    (assoc a j ai)
                    (assoc i aj))]
            (recur i (inc s) b)
            ))))))

(defn minimumBribes
  [q]
  (->>
    (reduce (fn [[i bribes] qi]
              (let [offset (- qi (+ i 1))]
                (if (> offset 2)
                  (reduced "Too chaotic")
                  (let [j (max 0 (- qi 2))]
                    [(dec i) (+ bribes (reduce (fn [acc j]
                                                 (if (> (nth q j) qi)
                                                   (inc acc)
                                                   acc))
                                               0
                                               (range j i)))]))))
            [(- (count q) 1) 0]
            (reverse q))
    (#(if (= (count %) 2)
        (println (second %))
        (println %)))))


(defn minimumBribes2
  [q]
  (loop [i (- (count q) 1)
         bribes 0]
    (if (>= i 0)
      (let [qi (nth q i)]
        (if (> (- qi (+ i 1)) 2)
          (println "Too chaotic")
          (recur (dec i)
                 (->> (loop [j (max 0 (- qi 2))
                             acc 0]
                        (if (< j i)
                          (if (> (nth q j) qi)
                            (recur (inc j) (inc acc))
                            (recur (inc j) acc))
                          acc))
                      (+ bribes)))))
      (println bribes))))

(defn rotLeft [a n d]
  (apply conj (vec (drop d a)) (drop-last (- n d) a)))

(defn rotLeft2 [a d]
  (if (> d 0)
    (let [h (first a)
          t (rest a)]
      (rotLeft2 (conj (vec t) h) (dec d)))
    a))
