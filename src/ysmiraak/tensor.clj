(ns ysmiraak.tensor "playing with your tensors may cause blindness.")

(defn mapx
  "like [mapv](https://clojuredocs.org/clojure.core/mapv) but for
  tensors, aka higher rank vectors in this implementation.  anything
  that's not a [vector?](https://clojuredocs.org/clojure.core/vector_q)
  is treated as a rank-0 tensor, aka a scalar of sorts.

  the first argument *r* is a non-negative integer, which is the rank
  of the tensors to which it applies the function *f* across.

  ```clojure
  (mapx 2 inc [[0 1] [2 3]])               => [[1 2] [3 4]]
  (mapx 2  +  [[0 1] [2 3]] [[1 2] [3 4]]) => [[1 3] [5 7]]
  ```

  it does not have to be applied to the lowest level where the scalars
  live.  for a rank-r tensor, any integer between *0* and *r*
  inclusive is applicable.

  ```clojure
  (mapx 1 peek [[0 1] [2 3]]) => [1 3]
  (mapx 0 str  [[0 1] [2 3]]) => \"[[0 1] [2 3]]\"
  ```

  it may be more intuitive to think of *r* as the axis instead of the
  rank.  except that a rank-0 tensor has no axis but `r = 0` is still
  valid, and, like indices, axes are not inclusive on both ends.

  note that the arity-2 case does not produce a transducer, since it
  operates on vectors and not lazy sequences.  instead it produces a
  partial function.

  see [[plux]] for a similar function which supports negative indexing
  and broadcasting semantics.  also see [[mulx]] and [[trax]] for
  tensor algebra operations.

  "
  ([r f]
   (if (pos? r)
     (recur (dec r) (partial mapv f))
     f))
  ([r f x]
   ((mapx r f) x))
  ([r f x & xs]
   (apply (mapx r f) x xs)))

(defn dims
  "returns the shape `(: Vect r Nat)` of tensor *x*, or reshapes it to
  new shape *ds*, if its size can be preserved.

  unlike common reshaping rules which conveniently allow one -1 in the
  shape to specify an unknown dimension to be dynamically filled, the
  new shape here may contain any number of negative integers.  any
  unassigned size is distributed among the negative axis according to
  their relative magnitude.

  for example with `(dims ? (vec (range 6)))` the shape `[-2 3 -1]`
  produces a tensor of shape `[2 3 1]`, wheras `[-1 3 -1]` gets an
  exception message.

  the purpose of this design is to simplify the logic of the
  implementation and to avoid special handling for edge cases, for
  example what happens when -1 meets a tensor of size zero, and what
  happens when multiple -1s are given but the remaining positive
  dimensions fill the size perfectly.

  for other common ndarray manipulations, see [[rank]] for
  transposition, [[size]] for broadcasting, and [[subx]] for slicing
  and indexing.

  "
  ([x]
   (loop [ds (transient []), x x]
     (if (vector? x)
       (recur (conj! ds (count x)) (peek x))
       (persistent! ds))))
  ([ds x]
   (let [dx (dims x), m (reduce * dx)
         [n q aqs] (reduce-kv
                    (fn [[n q aqs] i d]
                      (if (neg? d)
                        [n (- q d) (conj aqs [i (- d)])]
                        [(* d n) q aqs]))
                    [1 0 []] ds)
         fil (if (or (zero? n) (zero? q)) 0 (max (quot (quot m n) q) 1))
         ds' (reduce (fn [ds [a q]] (assoc ds a (* fil q))) ds aqs)]
     (cond (= ds' dx) x
           (zero? m) (if (or (zero? n) (not (zero? q)))
                       (reduce #(vec (repeat %2 %1)) [] (reverse (take-while pos? ds)))
                       (throw (ex-info "old dims has size 0 but not new dims" {:old dx :new ds})))
           (= (reduce * ds') m)
           , (cond-> ((->> (partial apply concat)
                           (repeat (dec (count dx)))
                           (apply comp vec)) x)
               (< 1 (count ds'))
               ((->> (next ds')
                     (map (partial partial partition))
                     (interleave (repeat (partial map vec)))
                     (apply comp vec))))
           :else (throw (ex-info "incompatible dims" {:old dx :new ds}))))))

(defn rank
  "returns the rank `(: Nat)` of tensor *x*, or changes the ordering of
  its axes according to *perm*, aka transposition."
  ([x]
   (loop [r 0, x x]
     (if (vector? x)
       (recur (inc r) (peek x))
       r)))
  ([perm x] ; todo support negative indexing here ???? and sanity check perm
   (loop [sidx (vec (sort-by perm (range (count perm)))), x x]
     (if-let [[[r i j]] (seq (drop-while (fn [[r i j]] (< i j)) (map vector (range) sidx (next sidx))))]
       (recur (assoc sidx (inc r) i r j) (mapx r (partial apply mapv vector) x))
       x))))

(defn size
  "returns the size `(: Nat)` of tensor *x*, or changes its size by
  broadcasting to shape *ds*, if the broadcasting logic is satisfied."
  ([x] (reduce * (dims x)))
  ([ds x]
   (let [ds' (dims x), pad (- (count ds) (count ds'))]
     (if (or (< pad 0) (some not (map (fn [d d'] (and d (or (= 1 d') (= d d') (= -1 d)))) (rseq ds) (rseq ds'))))
       (throw (ex-info "cannot broadcast old dims to new dims" {:old ds' :new ds})))
     (loop [r (dec (count ds))
            [d  & ds ] (rseq ds)
            [d' & ds'] (concat (rseq ds') (repeat pad 1))
            x (nth (iterate vector x) pad)]
       (if (<= 0 r)
         (recur (dec r) ds ds'
                (cond->> x
                  (and (not= d d') (pos? d))
                  (mapx r (comp vec (partial repeat d) peek))))
         x)))))

(defn inat
  "ensures that integer *i* is positive.  used for implementing negative
  indexing.  works under the condition that `-d-1 < i < d`.  see
  [[jnat]] for an inclusive variant."
  [d i]
  (if (< (bit-not d) i d)
    (mod i d)
    (throw (ex-info "index out of bound" {:index i :bound d}))))

(defn jnat
  "like [[inat]] but inclusive, namely `-d-1 <= i <= d`.  used for
  implementing negative slicing."
  [d i]
  (if (<= (bit-not d) i d)
    (mod i (inc d))
    (throw (ex-info "slice out of bound" {:slice i :bound d}))))

(defn subx
  "for subscripting.  each *sub* can be one of the following.

  - an integer, for indexing.  this collapses the axis it's applied to
  thus reduces the tensor rank.

  - a map with `:i :j :k` keys, for (strided) slicing.  the three
  values are integers, and work analogously to the arguments of
  [range](https://clojuredocs.org/clojure.core/range), also in the
  default cases with missing keys.  the default values allows an empty
  map to slice the whole axis.

  - a sequence of booleans, for masking.  the sequence needs not be a
  vector, but must have the same length as the dimension of that axis.

  - a sequence of integers, which returns the entries along that axis
  in the specified order, allowing duplicates.  an empty sequence
  returns an empty axis.  this is the most generic case.  any input
  not of the previous cases is interpreted this way.  for example
  `nil` is treated as an empty sequence.

  negative indices are supported in all cases where integers are used.
  but index-out-of-bound errors are not tolerated.  the *subs* are
  applied consequetively from the outer-most axis to the inner-most.
  use `{}` to leave some axis untouched.

  "
  ([x] x)
  ([x & subs]
   (loop [[sub & subs] subs, x x, [d & ds] (dims x), r 0]
     (as-> x x
       (cond
         (integer? sub) (as-> (inat d sub) i (mapx r #(nth % i) x))
         (map? sub) (as-> sub {:keys [i j k] :as sub}
                      {:i (jnat d (or i 0))
                       :j (jnat d (or j d))
                       :k         (or k 1)}
                      (if (= {:i 0 :j d :k 1} sub)
                        x (mapx r (cond
                                    (=  1 k) #(subvec % i j)
                                    (= -1 k) #(vec (rseq (subvec % i j)))
                                    (pos? k) #(vec (take-nth k (subvec % i j)))
                                    :else (as-> (vec (range i j k)) idxs #(mapv % idxs)))
                                x)))
         :else (as-> sub idxs
                 (if (boolean? (first idxs))
                   (if (= (count idxs) d)
                     (keep-indexed (fn [i x] (if x i)) idxs)
                     (throw (ex-info "boolean mask does not fit axis size" {:mask idxs :size d})))
                   idxs)
                 (mapv (partial inat d) idxs)
                 (mapx r #(mapv % idxs) x)))
       (if subs
         (recur subs x ds (if (integer? sub) r (inc r)))
         x)))))

(defn plux
  "like [[mapx]] but supported broadcasting and negative indexing."
  ([r f x] (mapx (jnat (rank x) r) f x))
  ([r f x & xs]
   (let [xs (cons x xs)      dss (map dims xs)
         rs (map count dss)  pad (vec (repeat (reduce max rs) 1))
         ds (apply mapv (fn [& ds] (as-> (reduce max ds) d (if (every? #(or (= 1 %) (= d %)) ds) d)))
                   (map (fn [ds r] (into (subvec pad r) ds)) dss rs))]
     (apply mapx (jnat (count ds) r) f
            (map (partial size ds) xs)))))

(defn mulx
  "tensor product.  except that only axes above rank *r* are affected."
  [r f x & xs] ; todo spare the first r ranks as documented
  (let [xs (vec (cons x xs))
        rs (map rank xs), m (reduce max rs), n (- m (jnat m r))
        rs (mapv #(max 0 (- % n)) rs)
        [r & ps] (->> rs rseq (reductions + 0) next reverse)]
    (->> (-> (fn [x r p] (mapx r (apply comp (repeat p vector)) x))
             (mapv xs rs ps)
             (conj (peek xs)))
         (apply plux r f))))

(defn trax
  "trace, or tensor contraction.

  ```
  f : (Vect n s) ^ m -> t
  a : (Vect q (Vect m int))
  x : (Tsor r s)
   -> (Tsor ((r - q) * m) t)
  ```

  *a* for axis is a rank-`[0,2]` tensor broadcastable to `[q m]` where
  *q* is the number of axes being contracted simultanously and *m* the
  number of input tensors.  each row in *a* specifies the axes of
  contraction for all input tensors.

  when *a* is a vector of integers, by broadcasting logic, only one
  axis is contracted from each tensor.  the same applies when it's
  just an integer, which however may point to different axes for different
  tensors, should it be a negative index.

  "
  [f a x & xs]
  (let [xs (cons x xs)    dss (map dims xs)    rs (mapv count dss)
        a  (size [-1 (count rs)] a)    q (nth (dims a) 0)
        [r & ps] (->> rs rseq (map #(- % q)) (reductions + 0) reverse)]
    (->> (rank [1 0] a)
         (map (fn [x r p ds a]
                (let [a1 (mapv (partial inat r) a)
                      a0 (filterv (complement (set a1)) (range r))]
                  (-> (mapv ds a0)
                      (into (repeat p 1))
                      (conj (reduce * (map ds a1)))
                      (dims (rank (into a0 a1) x)))))
              xs rs ps dss)
         (apply plux r f))))




;; todo
;; - tests
;; - where gather boolean_mask
