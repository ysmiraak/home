(ns ysmiraak.lang)

;; https://ncatlab.org/nlab/show/pure+type+system

(defrecord V [name])            (defn v? [x] (instance? V x))
(defrecord A [vars expr args])  (defn a? [x] (instance? A x))
(defrecord F [vars expr pars])  (defn f? [x] (instance? F x))
;; vars : Set V
;; pars : Vec V
;; args : Vec Expr

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

(defn- ff [expr pars]
  (-> (reduce disj (fv expr) pars)
      (F. expr (vec pars))))

(defn- aa [expr args]
  (-> (reduce into (fv expr) (map fv args))
      (A. expr (vec args))))

(extend-protocol Expr
  F
  (fv [this] (:vars this))
  (ev [{:keys [vars expr pars]} dict]
    (let [dict (select-keys dict vars)
          expr (cond-> expr (seq dict) (ev dict))]
      (cond
        ;; F without pars evals to its expr
        (empty? pars) expr
        ;; uncurries immediately nested F
        (f? expr) (ff (:expr expr) (into pars (:pars expr)))
        :else     (ff        expr        pars))))
  A
  (fv [this] (:vars this))
  (ev [{:keys [vars expr args]} dict]
    (let [dict (select-keys dict vars)
          mbev #(cond-> % (some (fv %) (keys dict)) (ev dict))
          expr (mbev expr)]
      (if (f? expr)
        ;; asap beta-reduces val/var args
        (let [{:keys [pars expr]} expr
              ;; todo
              ;; - eval args in parallel
              ;; - skip eval if par not in fv
              ;; - skip eval if par is overwritten
              [args pars dict]
              , (loop [args (seq args)  args' []
                       pars (seq pars)  pars' []
                       dict {}]
                  (if-let [[arg & args] args]
                    (let [arg (mbev arg)]
                      (if-let [[par & pars] pars]
                        (if (or (v? arg) (empty? (fv arg))) ; todo or when par is used only once in expr
                          (recur args       args'      pars       pars'      (assoc  dict par arg))
                          (recur args (conj args' arg) pars (conj pars' par) (dissoc dict par)))
                        [(into (conj args' arg) (map mbev args)) pars' dict]))
                    [args' (into pars' pars) (reduce dissoc dict pars)]))
              expr (-> (ff expr pars)
                       (ev dict))]
          (if (seq args)
            (aa expr args)
            expr))
        ;; defers application or applies primitive expr
        (let [args (pmap mbev args)]
          ((if (some (comp seq fv) (cons expr args)) aa apply)
           expr args)))))
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
  ([expr & args]
   (-> (aa expr args)
       (ev {}))))

(defn %
  "forall"
  ([expr & pars] ; currently pars can only be vars
   (-> (ff expr pars)
       (ev {}))))

(defn !
  "produces a fresh or named var"
  ([name] (V. name))
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

  ;; todo
  ;; - exception propogation
  ;; - test recursion
  ;; - types

  )
