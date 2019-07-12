(ns ysmiraak.tensor)

(defn mapx
  ([r f]
   (if (pos? r)
     (recur (dec r) (partial mapv f))
     f))
  ([r f x]
   ((mapx r f) x))
  ([r f x & xs]
   (apply (mapx r f) x xs)))

(defn- fit-dims
  [n ds]
  (let [[as m] (reduce-kv (fn [[as m] i d] (if (= -1 d) [(conj as i) m] [as (* d m)])) [[] 1] ds)
        ds' (reduce (partial apply assoc) ds (map vector as (repeat (if (zero? m) 0 (quot n m)))))]
    (cond ; todo find a better way to implement the logic
      (and (zero?  m) (< 0 (count as))) (throw (ex-info "ambiguous -1 when 0 present" {:dims ds}))
      (and (not= n m) (< 1 (count as))) (throw (ex-info "multiple ambiguous unknowns" {:dims ds}))
      (not= n (reduce * ds'))           (throw (ex-info "incompatible dims"   {:size n :dims ds}))
      :else ds')))

(defn- nat
  [d i]
  (if (neg? i)
    (+ i d)
    i))

(declare rank size dims dimx flax flix subx)

(defn rank
  ([x] (rank x 0))
  ([x r]
   (if (vector? x)
     (recur (peek x) (inc r))
     r)))

(defn size
  ([x] (reduce * (dims x)))
  ([x a]
   (as-> (dims x a) d
     (if-not (integer? d)
       (reduce * d)
       d))))

(defn dims
  ([x]
   (loop [ds (transient []), x x]
     (if (vector? x)
       (recur (conj! ds (count x)) (peek x))
       (persistent! ds))))
  ([x a] (subx (dims x) a)))

(defn dimx
  ([x] (flax x))
  ([x ds]
   (let [ds' (dims x), n (reduce * ds'), ds (fit-dims n ds)]
     (cond
       (= ds' ds) x
       (zero? n) (reduce (fn [v d] (vec (repeat d v))) [] (reverse (take-while pos? ds)))
       :else (cond-> (flax x (count ds'))
               (< 1 (count ds))
               ((->> (next ds)
                     (map (partial partial partition))
                     (interleave (repeat (partial map vec)))
                     (apply comp vec))))))))

(defn flax
  ([x] (flax x (rank x)))
  ([x r]
   ((->> (partial apply concat)
         (repeat (dec r))
         (apply comp vec))
    x)))

(defn flix
  ([x] (apply mapv vector x))
  ([x perm]
   (loop [x x idx (vec (sort-by perm (range (count perm))))]
     (if-let [[[a i j]] (seq (drop-while (fn [[a i j]] (< i j)) (map vector (range) idx (next idx))))]
       (recur (mapx a flix x) (assoc idx (inc a) i a j))
       x)))
  ([x a b & abs]
   (as-> (list* a b abs) abs
     (mapv (partial nat (rank x)) abs)
     (reduce (fn [x [a b]] (assoc x a (x b) b (x a)))
             (->> abs (reduce max) inc range vec)
             (partition 2 abs))
     (flix x abs))))

(defn subx
  ([x] x)
  ([x & subs] ; sub : int | {i j k} | [int] | [bool]
   (loop [[sub & subs] subs, x x, [d & ds] (dims x), r 0]
     (as-> x x
       (cond
         (integer? sub) (as-> (nat d sub) i (mapx r #(nth % i) x))
         (map? sub) (as-> sub {:keys [i j k] :as sub}
                      {:i (nat d (or i 0))
                       :j (nat d (or j d))
                       :k        (or k 1)}
                      (if-not (= {:i 0 :j d :k 1} sub)
                        (mapx r (cond
                                  (=  1 k) #(subvec % i j)
                                  (= -1 k) #(vec (rseq (subvec % i j)))
                                  (pos? k) #(vec (take-nth k (subvec % i j)))
                                  :else (as-> (vec (range i j k)) idxs #(mapv % idxs)))
                              x)
                        x))
         :else (as-> sub idxs
                 (cond-> idxs (boolean? (first idxs)) (keep-indexed (fn [i x] (if x i))))
                 (mapv (partial nat d) idxs)
                 (mapx r #(mapv % idxs) x)))
       (if subs
         (recur subs x ds (if (integer? sub) r (inc r)))
         x)))))

;; todo
;; - tests
;; - broadcasting
;; - tf.where tf.gather tf.boolean_mask
;; - tensor product & contraction
