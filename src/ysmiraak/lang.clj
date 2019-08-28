(ns ysmiraak.lang)

;; https://ncatlab.org/nlab/show/pure+type+system

(defrecord V [name])            (defn v? [x] (instance? V x))
(defrecord A [vars form])       (defn a? [x] (instance? A x))
(defrecord F [vars expr args])  (defn f? [x] (instance? F x))
;; vars : Set V
;; args : Vec V
;; form : Vec Expr

(defprotocol Expr
  "an expression satisfies the following protocol.

  - fv returns the set of free variables
  - ev evaluates the expression given dict, a map of variable bindings

  expression types and evaluation types.

  - V  variable     => Expr
  - A  application  => Expr
  - F  function     => F
  - _  value        => _

  "
  (fv [this])
  (ev [this dict]))

(extend-protocol Expr
  F
  (fv [this] (:vars this))
  (ev [{:keys [vars expr args]} dict]
    (let [dict (reduce dissoc dict args)
          expr (cond-> expr (some vars (keys dict)) (ev dict))]
      (if (f? expr) ; uncurries
        (F. (reduce disj (fv expr) args) (:expr expr) (into args (:args expr)))
        (F. (reduce disj (fv expr) args)        expr        args))))
  A
  (fv [this] (:vars this))
  (ev [{:keys [vars form]} dict]
    (let [[head & body :as form] (cond->> form (some vars (keys dict)) (mapv #(ev % dict)))]
      (if (f? head)
        (let [{:keys [vars expr args]} head
              dict (zipmap args body)
              size (count body)]
          (case (compare (count args) size) ; beta-reduce or curry
            0     (ev expr dict)
            1 (F. (reduce disj vars (keys dict))
                  (ev expr dict)
                  (subvec args size))
            (throw (ex-info "arity error" {:args args :vals body}))))
        (let [vars (apply clojure.set/union (map fv form))]
          (if (seq vars) ; defer application
            (A. vars form)
            (apply head body))))))
  V
  (fv [this] #{this})
  (ev [this dict] (dict this this))
  Object (fv [this] #{}) (ev [this dict] this)
  nil    (fv [this] #{}) (ev [this dict] this))

(defn ?%
  "takes a expr and interleaving vars and values, (partially) evaluates
  the expr according to the binding.

  similar to let, this can be seen as performing a lambda-abstraction
  immediately followed by a beta reduction (a left-left lambda).

  ```
  (?% phi a x b y)     (let [a x b y] phi)
  (? (% phi a b) x y)  ((fn [a b] phi) x y)
  ```

  "
  [expr & {:as dict}]
  (ev expr dict))

(defn ?
  "funcall, the counit of product-exponential adjunction"
  ([& form]
   (-> (apply clojure.set/union (map fv form))
       (A. (vec form))
       (ev {})))
  ([]))

(defn %
  "forall"
  ([expr & args] ; currently args can only be vars
   (-> (reduce disj (fv expr) args)
       (F. expr (vec args))
       (ev {})))
  ([expr] expr))

(defn !
  "produces a fresh or named var"
  ([name] (V. name))
  ;; todo use number instead of gensym
  ([] (! (. clojure.lang.RT (nextID)))))

(comment

  (def a (! 0))
  (def b (! 1))
  (def c (! 2))

  (def phi (? + a b))
  (def chi (% phi b))
  (def psi (% chi a))

  ;; uncurrying
  (= (-> phi (% b) (% a))
     (-> phi (% a b)))

  ;; currying
  (= (? (? psi 2) 2)
     (? psi 2 2)
     4)

  ;; partial eval
  (=    (? + a 2)          (? chi 2))
  (= (% (? + a 2) a)    (% (? chi 2) a))
  (=    (? + 2 2)    (? (% (? chi 2) a) 2))
  (= 4 (? (% (? apply ? chi [2]) a) 2))

  ;; swap var
  (= (% (? psi b c) b c)
     (% (? + b c) b c))
  (= (?% (? + a b) a b)
     (? + b b))

  ;; swap var for expr
  (= (?% (? + a b) a (? * b b))
     (? + (? * b b) b))

  ;; preserve laziness (which eval doesn't)
  (? map (partial ? psi) (range) (range))

  ;; non-terminating expr
  (? (% (? ? a a) a)
     (% (? ? a a) a))

  ;; todo test recursion

  )
