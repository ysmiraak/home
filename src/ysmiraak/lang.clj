(ns ysmiraak.lang)

;; https://ncatlab.org/nlab/show/pure+type+system

(defrecord V [name])            (defn v? [x] (instance? V x))
(defrecord A [vars expr args])  (defn a? [x] (instance? A x))
(defrecord F [vars expr pars])  (defn f? [x] (instance? F x))
;; vars : Map (Set V) Pos
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
  (-> (reduce dissoc (fv expr) pars)
      (F. expr (vec pars))))

(defn- aa [expr args]
  (-> (apply merge-with + (fv expr) (map fv args))
      (A. expr (vec args))))

(extend-protocol Expr
  F
  (fv [this] (:vars this))
  (ev [{:keys [vars expr pars]} dict]
    (let [dict (select-keys dict (keys vars))
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
    (let [dict (select-keys dict (keys vars))
          mbev #(cond-> % (some (fv %) (keys dict)) (ev dict))
          expr (mbev expr)]
      (if (f? expr)
        ;; asap beta-reduces val/var args
        (let [{:keys [pars expr]} expr  vars (fv expr)
              ;; disable shadowed pars
              pars (loop [raps (rseq pars)  rs #{}  res ()]
                     (if-let [[r & raps] raps]
                       (recur raps (conj rs r) (cons (and (not (rs r)) r) res))
                       (vec res)))
              ;; offset pars & args
              len (min (count pars) (count args))
              [pars pars'] (split-at len pars)
              [args args'] (split-at len args)
              ;; delay or subst
              [pars args dict]
              , (loop [pas (-> (fn [p a]
                                 (when-let [a (and p (vars p) (mbev a))]
                                   ;; ev a when p not shadowed and in vars
                                   (if (or (v? a) (empty? (fv a)) (= 1 (vars p)))
                                     ;; subst when: a var | a val | p used once
                                     {p a}
                                     [p a])))
                               (pmap pars args)
                               seq)
                       pars []  args []  dict {}]
                  (if-let [[pa & pas] pas]
                    (cond (vector? pa) (recur pas (conj pars (pa 0)) (conj args (pa 1)) dict)
                          (map?    pa) (recur pas pars args (into dict pa))
                          :else        (recur pas pars args dict))
                    [pars args dict]))
              pars (into pars pars')
              args (into args args')
              ;; subst
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
  (fv [this] {this 1})
  (ev [this dict] (dict this this))
  Object (fv [this] {}) (ev [this dict] this)
  nil    (fv [this] {}) (ev [this dict] this))

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
