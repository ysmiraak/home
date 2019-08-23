(ns ysmiraak.lang
  (:require [clojure.core.match :refer [match]]))

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

(defrecord Term   [expr vars])
(defrecord Forall [expr args])

(defn forms->term
  "merges multiple forms into a single term"
  [forms]
  (loop [forms (seq forms)  expr []  vars #{}]
    (if-let [[form & forms] forms]
      (if (instance? Term form) ; build expr & collect vars
        (recur forms (conj expr (:expr form)) (into vars (:vars form)))
        (recur forms (conj expr        form)        vars))
      (Term. (seq expr) vars))))

(defn subst
  "substitutes all keys in expr to vals in env"
  [env expr]
  (cond ; todo use a better data structure (lens?) for this
    (seq? expr)             (map (partial subst env) expr)
    (instance? Forall expr) (assoc expr :expr (subst (reduce dissoc env (:args expr)) (:expr expr)))
    :else                   (env expr expr)))

(defn beta
  "beta-reduces a term whose expr has a Forall as head"
  [{[{:keys [expr args]} & vals] :expr vars :vars}]
  (loop [rgs (seq args)  als (seq vals)  env {}]
    (match [rgs als]
           [nil nil] (Term. (subst env expr) (reduce disj vars (keys env)))
           [_   nil] (Term. (Forall. (subst env expr) rgs) (reduce disj vars (keys env)))
           [nil   _] (throw (ex-info "arity error" {:args args :vals vals}))
           :else (let [[a & rgs] rgs
                       [v & als] als]
                   (recur rgs als (assoc env a v))))))

(defn ?
  "funcall, the counit of product-exponential adjunction"
  ([& forms]
   (as-> (forms->term forms) {:keys [expr vars] :as term}
     (cond-> term (instance? Forall (first expr)) beta)
     (if (seq vars) term (eval expr))))
  ([]))

(defn %
  "forall"
  [form & args] ; currently args can only be var terms
  (let [term (if (instance? Term form) form (Term. form #{}))
        expr (:expr term)
        args (map :expr args)]
    (Term.
     (if (instance? Forall expr)
       (Forall. (:expr expr) (concat args (:args expr)))
       (Forall.        expr          args))
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

;; todo goal 1
(comment

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
