(ns ysmiraak.lang)

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

(defrecord Term   [expr vars])
(defrecord Forall [expr args])

(defn term?   [x] (instance? Term   x))
(defn forall? [x] (instance? Forall x))

(defn forms->term
  "merges multiple forms into a single term"
  [& forms]
  (loop [forms forms  expr []  vars #{}]
    (if-let [[form & forms] forms]
      (if (term? form)
        (recur forms (conj expr (:expr form)) (into vars (:vars form)))
        (recur forms (conj expr        form)        vars))
      (Term. (seq expr) vars))))

(defn subst
  "substitutes all keys in expr to vals in env (: map var val)"
  [env expr]
  (cond ; consider using a better data structure (lens?) for this
    (seq?    expr) (map (partial subst env) expr)
    (forall? expr) (assoc expr :expr (subst (reduce dissoc env (:args expr)) (:expr expr)))
    :else          (env expr expr)))

(defn beta
  "beta reduces a term whose expr has a Forall as head"
  [{:keys [expr vars] :as term}]
  (if (and (seq? expr) (forall? (first expr)))
    (let [[{:keys [expr args]} & vals] expr  nval (count vals)  env (zipmap args vals)]
      (case (compare (count args) nval)
        1 (Term. (Forall. (subst env expr) (subvec args nval)) (reduce disj vars (keys env)))
        0 (Term.          (subst env expr)                     (reduce disj vars (keys env)))
        (throw (ex-info "arity error" {:args args :vals vals}))))
    term))

(defn ev [expr]
  ;; todo this approach breaks when lists are used as data
  ;; - different types of expr for data and code
  ;; - manual recursive ev
  (eval expr))

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
  ([{:keys [expr vars] :as term}]
   (if (seq vars) term (ev expr)))
  ([{:keys [expr vars]} & {:as envt}]
   (let [args (map :expr (keys envt))
         expr (subst (zipmap args (vals envt)) expr)
         vars (reduce disj vars args)]
     (?% (Term. expr vars)))))

(defn ?
  "funcall, the counit of product-exponential adjunction"
  [form & forms]
  (-> (apply forms->term form forms)
      beta
      ?%))

(defn %
  "forall"
  [form & args] ; currently args can only be var terms
  (let [term (if (term? form) form (Term. form #{}))
        expr (:expr term)
        args (mapv :expr args)]
    (Term.
     (if (forall? expr)
       (Forall. (:expr expr) (into args (:args expr)))
       (Forall.        expr        args))
     (reduce disj (:vars term) args))))

(defn !
  "produces a fresh or named var term"
  ([name] (Term. name #{name}))
  ([] (! (gensym "!"))))

(comment

  (def a (! 'a))
  (def b (! 'b))
  (def c (! 'c))

  (? inc 0)
  ;; 1
  (? inc a)
  ;; Term {
  ;;  :expr (inc a)
  ;;  :vars #{a}
  ;; }
  (? + (inc 0) a)
  ;; Term {
  ;;   :expr (+ 1 a)
  ;;   :vars #{a}
  ;; }
  (? inc (? inc a))
  ;; Term {
  ;;   :expr (inc (inc a))
  ;;   :vars #{a}
  ;; }

  (def phi (? + a b))
  ;; Term {
  ;;   :expr (+ a b)
  ;;   :vars #{a b}
  ;; }
  (def chi (% phi b))
  ;; Term {
  ;;   :expr Forall {
  ;;           :term (+ a b)
  ;;           :args [b]
  ;;         }
  ;;   :vars #{a}
  ;; }
  (def psi (% chi a))
  ;; Term {
  ;;   :expr Forall {
  ;;           :term (+ a b)
  ;;           :args [a b]
  ;;         }
  ;;   :vars #{}
  ;; }

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
