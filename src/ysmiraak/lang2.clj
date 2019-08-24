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

;; term =  val | form
;; form = expr & vars
;; expr =  var | beta | forall
;;
;; expr must be wrapped in form, otherwise it is treated as val
;; binding a var to a form changes everything
;;
;; val    => val
;; var    => val | form *
;; form   => val | form
;; beta   => val | form *
;; forall => form forall

(defrecord Form   [expr vars])    (defn form?   [x] (instance?  Form   x))
(defrecord Var    [name])         (defn var?    [x] (instance?  Var    x))
(defrecord Beta   [sexp])         (defn beta?   [x] (instance?  Beta   x))
(defrecord Forall [term args])    (defn forall? [x] (instance?  Forall x))
(defprotocol Expr)                (defn expr?   [x] (satisfies? Expr   x))
(extend-protocol Expr Var Beta Forall)

(defprotocol Term
  "

  ev

  - substitutes all keys in expr to vals in env (: map var val)
  - beta reduces a term whose expr has a Forall as head

  "
  (ev [this vars env]))

(extend-protocol Term

  Form
  ;; - val if its content reduces to val
  ;; - form * with newly collected free vars
  ;; todo guard from ev
  (ev [{:keys [expr vars]} vars env]
    (ev expr (reduce disj vars (keys env)) env))

  Var
  ;; - val
  ;; - form *
  (ev [this _ env] (env this this))

  Beta
  ;; - val
  ;; - form *
  (ev [{sexp :sexp} vars env]
    (let [;; evalutes sexp if necessary
          ;; env (select-keys env vars)
          [head & body :as sexp] (cond->> sexp (seq env) (map #(ev % vars env)))
          vars (reduce disj vars (keys env))]
      (if (and (form? head) (forall? (:expr head)))
        (let [{:keys [expr vars]} head
              {:keys [term args]} expr
              env (zipmap args body)
              num (count body)]
          (case (compare (count args) num)
            ;; curry
            1 (-> (ev term vars env)
                  (Forall. (subvec args num))
                  (Form. (reduce disj vars (keys env))))
            ;; beta-reduce
            0 (as-> (ev term vars env) term
                (if (expr? term)
                  (Form. term (reduce disj vars (keys env)))
                  term))
            (throw (ex-info "arity error" {:args args :vals body}))))
        (if (seq vars)
          (Form. (Beta. sexp) vars)
          (apply head body)))))

  Forall
  ;; - form forall
  (ev [{:keys [term args]} vars env]
    (let [env  (reduce dissoc env args) ; evalutes term if necessary
          term (cond-> term (seq env) (ev vars env))]
      (if (form? term)
        (if (forall? (:expr term))
          ;; uncurries
          (as-> term [{{:keys [term args']} :expr vars :vars}]
            (Form. (Forall. term (into args args'))
                   (reduce disj vars args)))
          (Form. (Forall. term args) (reduce disj (:vars term) args)))
        (Form. (Forall. term args) #{}))))

  Object (ev [this _ _] this)
  nil    (ev [this _ _] this))

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
  (ev term #{} env))

(defn ?
  "funcall, the counit of product-exponential adjunction"
  ([& terms]
   (let [terms (for [t terms] (if (var? t) (Form. t #{t}) t))
         vars (apply clojure.set/union (map :vars (filter form? terms)))]
     (ev (Beta. terms) vars {})))
  ([]))

(defn %
  "forall"
  ([term & args] ; currently args can only be vars
   (ev (Forall. term (vec args))
       (reduce disj (if (form? term) (:vars term) #{}) args)
       {}))
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
;; - fix (keys env)
;; - test how to replace var using env
