(ns ysmiraak.tensor)

(defn map*
  ([n k]
   (if (pos? n)
     (recur (dec n) (partial mapv k))
     k))
  ([n k tsr]
   ((map* n k) tsr))
  ([n k tsr & tsrs]
   (apply (map* n k) tsr tsrs)))

(defn shape
  [tsr]
  (loop [shape [] tsr tsr]
    (if (vector? tsr)
      (recur (conj shape (count tsr)) (peek tsr))
      shape)))

(defn slice
  ;; i         i
  ;; [     ]   :
  ;; [i    ]  i:
  ;; [i j  ]  i:j
  ;; [i j k]  i:j:k
  ([tsr] tsr)
  ([tsr & idxs]
   (loop [[idx & idxs] idxs, [d & ds] (shape tsr), axis 0, tsr tsr]
     (let [pos (fn [i] (if (neg? i) (+ d i) i))
           tsr (if (integer? idx)
                 (as-> (pos idx) i
                   (if (= (dec d) i) peek #(nth % i))
                   (map* axis i tsr))
                 (as-> idx [i j k :as idx]
                   [(pos (or i 0)) (pos (or j d)) (or k 1)]
                   (if-not (= [0 d 1] idx)
                     (map* axis
                           (cond
                             (=  1 k) #(subvec % i j)
                             (= -1 k) #(vec (rseq (subvec % i j)))
                             (pos? k) #(vec (take-nth k (subvec % i j)))
                             :else (as-> (vec (range i j k)) idxs
                                     (fn [v] (mapv #(nth v %) idxs))))
                           tsr)
                     tsr)))]
       (if idxs
         (recur idxs ds (+ axis (if (integer? idx) 0 1)) tsr)
         tsr)))))

(defn reshape
  [tsr dims]
  (let [dim$ (shape tsr)
        siz$ (reduce * dim$)
        fill (keep-indexed (fn [i d] (if (neg? d) i)) dims)
        fill (if (and (< 1 (count fill)) (not (zero? siz$)))
               (throw (ex-info "multiple unknown dimensions" {:dims dims}))
               (first fill))
        size (reduce * (cond-> dims fill (assoc fill 1)))
        dim' (cond-> dims fill (assoc fill (if (zero? size) 0 (quot siz$ size))))
        size (reduce * dim')
        dims (if-not (= siz$ size)
               (throw (ex-info "incompatible shapes" {:old dim$ :new dims}))
               dim')]
    (if-not (= dim$ dims)
      (if (zero? size)
        (if-not (= dim$ (take (count dim$) dims))
          (reduce (fn [v d] (vec (repeat d v))) [] (reverse (take-while pos? dims)))
          tsr)
        (as-> tsr tsr
          ((->> (partial apply concat)
                (repeat (dec (count dim$)))
                (apply comp vec)) tsr)
          (if (< 1 (count dims))
            ((->> (next dims)
                  (map (partial partial partition))
                  (interleave (repeat (partial map vec)))
                  (apply comp vec)) tsr)
            tsr)))
      tsr)))

(defn transpose
  ([tsr] (apply mapv vector tsr))
  ([tsr perm]
   (loop [tsr tsr idx (vec (sort-by perm (range (count perm))))]
     (if-let [[[a i j]] (seq (drop-while (fn [[a i j]] (< i j)) (map vector (range) idx (next idx))))]
       (recur (map* a transpose tsr) (assoc idx (inc a) i a j))
       tsr))))

;; todo
;; - index gather
;; - boolean mask
;; - np.where
