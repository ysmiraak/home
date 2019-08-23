(ns ysmiraak.lang
  (:refer-clojure :exclude [var?]))

;; goals
;; 1. unify terms and types (basic constructors shared by types and data)
;; 2. unify composition and application (????)
;; 3. compositional lambda calculus (variables and their evaluation rules)
;; 4. asap beta reduction (partial evaluation)

;; val  = data                -- what evaluates to itself
;; var  = symbol              -- what serves as a placeholder
;; expr = val | var | [expr]  -- what can be (partially) evaluated
;; form = expr | term
;; term = expr & vars
;; vars = set var
;; args = vec var

;; term = form | var | val
;; form = expr & vars
;; expr = beta | forall
;;
;; expr are always wrapped in form
;; vars only reduces during evaluation
;;
;; val    => val
;; var    => val | var
;; form   => form | var | val
;; beta   => form | beta | var | val
;; forall => forall

(defrecord Var    [name])         (defn var?    [x] (instance?  Var    x))
(defrecord Form   [expr vars])    (defn form?   [x] (instance?  Form   x))
(defrecord Beta   [sexp])         (defn beta?   [x] (instance?  Beta   x))
(defrecord Forall [term args])    (defn forall? [x] (instance?  Forall x))
(defprotocol Expr)                (defn expr?   [x] (satisfies? Expr   x))
(extend-protocol Expr Beta Forall)

(defprotocol Term
  "

  fv

  ev

  - substitutes all keys in expr to vals in env (: map var val)
  - beta reduces a term whose expr has a Forall as head

  "
  (fv [this])
  (ev [this env]))

(extend-protocol Term

  Var
  (fv [this] #{this})
  (ev [this env] (env this this))

  Form
  (fv [this] (:vars this))
  (ev [{:keys [expr vars]} env]
    (let [;; evaluates expr if necessary
          term (if (some vars (keys env)) (ev expr env) expr)
          vars (if (form? term) (:vars term) (reduce disj vars (keys env)))
          term (if (form? term) (:expr term) term)
          ;; now term is not a form
          term (if (and (beta? term) (empty? vars))
                 (as-> (:sexp term) [head & body]
                   (apply head body))
                 term)]
      (if (expr? term)
        (Form. term vars)
        term)))

  Beta
  (ev [this env]
    (as-> (:sexp this) [head & body :as sexp]
      ;; evalutes sexp if necessary
      (cond->> sexp (seq env) (map #(ev % env)))
      (if (and (form? head) (forall? (:expr head)))
        (let [{:keys [expr vars]} head
              {:keys [term args]} expr
              env (zipmap args body)
              num (count body)]
          (case (compare (count args) num)
            ;; curry
            1 (-> (ev term env)
                  (Forall. (subvec args num))
                  (Form. (reduce disj vars (keys env))))
            ;; beta-reduce
            0 (as-> (ev term env) term
                (if (expr? term)
                  (Form. term (reduce disj vars (keys env)))
                  term))
            (throw (ex-info "arity error" {:args args :vals body}))))
        (Beta. sexp))))

  Forall
  (ev [{:keys [term args]} env]
    (let [env  (reduce dissoc env args) ; evalutes term if necessary
          term (cond-> term (seq env) (ev env))]
      (if (and (form? term) (forall? (:expr term)))
        (as-> (:expr term) expr ; uncurries
          (assoc expr :args (into args (:args expr))))
        (Forall. term args))))

  Object
  (fv [this] #{})
  (ev [this env] this)

  nil
  (fv [this] #{})
  (ev [this env] this))

(defn ?%
  "takes a term and interleaving var-terms and values, (partially)
  evaluates the term according to the binding.

  similar to let, this can be seen as performing a lambda-abstraction
  immediately followed by a beta reduction (a left-left lambda).

  ```
  ((fn [a b] phi) x y)
  (let [a x b y] phi)
  (?% phi a x b y)
  (? (% phi a b) x y)
  ```

  "
  [term & {:as env}]
  (ev term env))

(defn ?
  "funcall, the counit of product-exponential adjunction"
  ([& terms]
   (as-> (ev (Beta. terms) {}) term
     (if (beta? term)
       (ev (Form. term (apply clojure.set/union (map fv terms))) {})
       term)))
  ([]))

(defn %
  "forall"
  ([term & args] ; currently args can only be vars
   (-> (Forall. term (vec args))
       (ev {})
       (Form. (reduce disj (if (form? term) (:vars term) #{}) args))
       (ev {})))
  ([term] term))

(defn !
  "produces a fresh or named var"
  ([name] (Var. name))
  ([] (! (gensym "!"))))

(comment

  (def a (! 'a))
  (def b (! 'b))
  (def c (! 'c))

  (def phi (? + a b))
  (def chi (% phi b))
  (def psi (% chi a))

  ;; binding
  (= (-> phi (% b) (% a))
     (-> phi (% a b)))

  ;; currying
  (= 4 (? psi 2 2))
  (= 4 (? (? psi 2) 2))

  ;; partial eval
  (=    (? + a 2)          (? chi 2))
  (= (% (? + a 2) a)    (% (? chi 2) a))
  (=    (? + 2 2)    (? (% (? chi 2) a) 2))

  (= 1 (? (% (? (% a a) 1) a) 0))
  (= 4 (? (% (? apply ? chi [2]) a) 2))

  (? map (partial ? psi) (range) (range))

  ;; non-terminating term
  (? (% (? ? a a) a) (% (? ? a a) a))

  ;; todo test recursion

  )

(comment ; todo goal 1

  ;; let
  ;; - A B C be types
  ;; - a b c be variables
  ;; - x y z be values

  ;; types all the way down
  ($ Type A x nil)

  ;; A -> B
  ($ Type
     (%'   A       B)
     (% ($ A a) ($ B (... a ...)))
     nil)

  ;; forall (a : A) . C(a)
  ($ Type
     (%'   A       Type)
     (% ($ A a)    (C a))
     (% ($ A a) ($ (C a) (... a ...)))
     nil)

  ;; forall (a : A , b : B) . C(a)
  ($ Type
     (%'   A       Type    Type)
     (% ($ A a)    B       (C a))
     (% ($ A a) ($ B b) ($ (C a) (... a ... b ...)))
     nil)

  ;; exists (a : A , b : B) . C(a)
  ($ Type
     (&'   A       Type    Type)
     (& ($ A a)    B       (C a))
     (& ($ A x) ($ B y) ($ (C x) (... x ... y ...)))
     nil)

  ;; exists (a : A) . C(a)
  ($ Type
     (&'   A       Type)
     (& ($ A a)    (C a))
     (& ($ A x) ($ (C x) (... x ...)))
     nil)

  ;; (A, B)
  ($ Type
     (&    A       B)
     (& ($ A x) ($ B y))
     nil)

  )

;; todo
;; - test how to replace var using env
