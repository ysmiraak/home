(ns ysmiraak.tensor "playing with your tensors may cause blindness.")

(defn mapx
  "like [mapv](https://clojuredocs.org/clojure.core/mapv) but for
  tensors, aka higher rank vectors in this implementation.  anything
  that's not a [vector?](https://clojuredocs.org/clojure.core/vector_q)
  is treated as a rank-0 tensor, aka a scalar of sorts.

  the first argument *r* is a non-negative integer, which is the rank
  of the tensors where the function *f* applies.

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
  partial function.  in general `(mapx 0 f)` is equivalent to `f`
  and `(partial mapx 1)` equivalent to `mapv`.

  see [[plux]] for a similar function which supports negative indexing
  and broadcasting semantics.

  "
  ([r f]
   (if (pos? r)
     (recur (dec r) (partial mapv f))
     f))
  ([r f x]
   ((mapx r f) x))
  ([r f x & xs]
   (apply (mapx r f) x xs)))

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

(defn rank
  "returns the rank `(: Nat)` of tensor *x*, or changes its rank to *r*
  by flattening or wrapping the outer-most axes."
  ([x]
   (loop [r 0  x x]
     (if (vector? x)
       (recur (inc r) (peek x))
       r)))
  ([r x] ; todo negative r changes the inner-most axes?
   (let [r' (rank x)  d (- r' r)]
     ((cond
        (pos? d) (do (if (<= r' d) (throw (ex-info "illegal rank" {:rank r})))
                     (apply comp vec (repeat    d (partial apply concat))))
        (neg? d)     (apply comp     (repeat (- d) vector))
        :else identity) x))))

(defn dims
  "returns the shape `(: Vect r Nat)` of tensor *x*, or reshapes it to
  new shape *ds*, if its size can be preserved.

  unlike common reshaping rules which conveniently allow one -1 in the
  shape to specify an unknown dimension to be dynamically filled, the
  new shape here may contain any number of negative integers.  any
  unassigned size is distributed among the negative axis according to
  their relative magnitude.

  for example with `(dims ? (vec (range 6)))` the shape `[-2 3 -1]`
  produces a tensor of shape `[2 3 1]`, whereas `[-1 3 -1]` gets an
  exception message.

  the purpose of this design is to simplify the logic of the
  implementation and to avoid special handling for edge cases, for
  example what happens when -1 meets a tensor of size zero, and what
  happens when multiple -1s are given but the remaining positive
  dimensions fill the size perfectly.

  for other common ndarray manipulations, see [[rank]] for
  flattening, [[size]] for broadcasting, [[flix]] for transposition,
  and [[subx]] for slicing and indexing.

  "
  ([x]
   (loop [ds (transient [])  x x]
     (if (vector? x)
       (recur (conj! ds (count x)) (peek x))
       (persistent! ds))))
  ([ds x]
   (let [dx (dims x)  m (reduce * dx)
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
           , (as-> (rank 1 x) x
               (case (count ds')
                 0 (peek x)  1 x
                 ((->> (next ds')
                       (mapv (partial partial partition))
                       (interleave (repeat (partial mapv vec)))
                       (apply comp vec))
                  x)))
           :else (throw (ex-info "incompatible dims" {:old dx :new ds}))))))

(defn size
  "returns the size `(: Nat)` of tensor *x*, or changes its size by
  broadcasting to shape *ds*, if the broadcasting logic is satisfied."
  ([x] (reduce * (dims x)))
  ([ds x]
   (let [ds' (dims x)  pad (- (count ds) (count ds'))]
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

(defn flix
  "for transposition.  the first case flips the 2 outer-most axes.
  the second case performs the specified permutation.  the third case
  takes an even number of axes and consecutively flips each two.

  "
  ([x] (apply mapv vector x))
  ([perm x]
   (let [r (count perm)  perm (mapv (partial inat r) perm)]
     (if (not= (set perm) (set (range r)))
       (throw (ex-info "invalid permutation" {:perm perm})))
     (loop [sidx (vec (sort-by perm (range r)))  x x]
       (if-let [[[r i j]] (seq (drop-while (fn [[r i j]] (< i j)) (map vector (range) sidx (next sidx))))]
         (recur (assoc sidx (inc r) i r j) (mapx r flix x))
         x))))
  ([x a b & abs]
   (as-> (list* a b abs) abs
     (mapv (partial inat (rank x)) abs)
     (reduce (fn [x [a b]] (assoc x a (x b) b (x a)))
             (->> abs (reduce max) inc range vec)
             (partition 2 abs))
     (flix abs x))))

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

  "
  ([x] x)
  ([x & subs]
   (loop [[sub & subs] subs  x x  [d & ds] (dims x)  r 0]
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
  "like [[mapx]] but supports broadcasting and negative indexing.

  this function can be used to broadcast tensors together.
  ```clojure
  (as-> (plux 0 vector x y z) [x y z] ... )
  ```

  "
  ([r f x] (mapx (jnat (rank x) r) f x))
  ([r f x & xs]
   (let [xs (cons x xs)       dss (mapv dims xs)
         rs (mapv count dss)  pad (vec (repeat (reduce max rs) 1))
         ds (apply mapv max (mapv (fn [ds r] (into (subvec pad r) ds)) dss rs))]
     (apply mapx (jnat (count ds) r) f
            (mapv (partial size ds) xs)))))

(defn tenx
  "tensor manipulation by axis annotation.

  ```
  f : Tsor s q -> t
  t : Vect _ q
  x : Tsor s r
  a : Vect _ r
   -> Tsor t (r - q)
  ```

  each rank-r tensor *x* is followed by a dim-r vector *a* which
  annotates its axes with tags.  an annotation tag is an arbitrary
  value, but must be unique within each annotation vector.  across
  annotation vectors, the same tag identifies the axes to be aligned.
  those axes must have broadcastable dimensions.

  *t* contains tags for extracting axes to which *f* applies.  those
  tags need not be unique, but only the last occurrence matters.  they
  may also be new tags, in which case dummy axes are created.

  if some axes survive extraction, their tags are included in the
  metadata of the resulting tensor under the `:axes` key.

  assuming the definition of those tensors in the following examples.
  ```clojure
  (def x (dims [4 3 2] (vec (range 24))))
  (def y (dims [4 3  ] (vec (range 12))))
  (def z (dims [  3 2] (vec (range  6))))
  ```

  - tensor product
  ```clojure
  (def xyz (tenx * [] x [:x0 :x1 :x2] y [:y0 :y1] z [:z0 :z1]))
  (dims xyz) => [4 3 2 4 3 3 2]
  (meta xyz) => {:axes [:x0 :x1 :x2 :y0 :y1 :z0 :z1]}
  ```

  - tensor contraction
  ```clojure
  (def dot (comp (partial reduce +) (partial map *)))
  (dims (tenx dot [3] x [4 3 2] y [4 3] z [3 2])) => [4 2]
  ```

  - operation along axis
  ```clojure
  (def sum (partial reduce +))
  (dims (tenx sum [1] x [0 1 2])) => [4 2]
  ```

  - broadcasting
  ```clojure
  (let [[x' e] (tenx list [4 3 2] x [4 3 2] 0 [])]
    (assert (= (dims x') (dims e)))
    (assert (identical? x' x)))
  ```

  - transposition
  ```clojure
  (dims (tenx identity [2 1 0] x [0 1 2])) => [2 3 4]
  ```

  - or this
  ```clojure
  (tenx identity [0 1 2 3] 0 []) => [[[[0]]]]
  ```

  "
  [f t x a & xa*]
  (let [[x+ a+] (apply mapv vector (partition 2 (list* x a xa*)))
        h (loop [a* (seq (remove (set t) (apply concat a+))) h {}]
            (if-let [[a & a*] a*]
              (recur a* (if (h a) h (assoc h a (count h))))
              (vec (sort-by h (keys h)))))
        ht (zipmap (concat h t) (range))
        p+ (mapv #(vec (sort-by (comp ht %) (range (count %)))) a+)
        ht (mapv key (sort-by val ht))
        d+ (mapv (fn [x a] (as-> (zipmap a (dims x)) ad (mapv #(or (ad %) 1) ht))) x+ a+)
        d' (apply mapv max d+)]
    (as-> x+ x+
      (mapv (fn [x p d] (->> x (flix p) (dims d) (size d'))) x+ p+ d+)
      (apply mapx (count h) f x+)
      (cond-> x+ (seq h) (with-meta {:axes h})))))

(defn fibx
  "tensor fiber.

  consider tensor type `Tsor (k : Type) (r : Nat) (d : Vect Nat r)`
  equivalently expressed as `fmap FinSet d -> k`; its fiber (inverse
  image) function type is `k -> PowSet (fmap FinSet d -> k)`; or
  ```
  Tsor k r = Vect Nat r -> k
  k -> Vect (Vect Nat r) ?
  ```
  with some simplification.

  now consider only truthy and falsey values of `k`, this function
  returns the truthy fiber, aka indices of truthy values in tensor *x*
  where each index is a vector of *r* natural numbers corresponding to
  the axes.  note that *0* and *0.0* are considered truthy.

  it is then possible to retrieve the fiber of arbitrary values by
  converting the values to booleans with [[plux]] or [[mapx]].

  "
  [x]
  (loop [dest (transient [])
         idxs (->> (interleave (map (comp vec range) (dims x))
                               (map vector (range)))
                   (apply tenx vector [])
                   (rank 2))
         mask (seq (rank 1 x))]
    (if mask
      (let [[m & mask] mask
            [i & idxs] idxs]
        (recur (cond-> dest m (conj! i)) idxs mask))
      (persistent! dest))))

;; todo tests
