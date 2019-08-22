(ns ysmiraak.lang)

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

;; goal
;; - unify terms and types (basic constructors shared by types and data)
;; - compositional lambda calculus (variables and their evaluation rules)
;; - no closure (asap beta reduction)
;; - unify composition and application (????)

(defrecord Term [form vars])

(defn ! []
  (as-> (gensym "!") v
    (Term. v #{v})))

(defrecord Forall [form args])

(defn % [term & args]
  (let [term (if (instance? Term term) term (Term. term #{}))
        {:keys [form vars]} term
        args (map :form args)]
    (Term.
     (if (instance? Forall form)
       (Forall. (:form form) (concat args (:args form)))
       (Forall.        form          args))
     (clojure.set/difference vars (set args)))))

(defn subst [env form]
  (if (seq? form)
    (map (partial subst env) form)
    (if (instance? Forall form)
      (assoc form :form (subst (apply dissoc env (:args form)) (:form form)))
      (get env form form))))

(defn ? [& expr]
  ;; val = anything that evaluates to itself
  ;; expr = val | [val] | term
  ;; term = form & vars
  ;; form = val | var | [form]
  ;; vars = set var
  ;; var = symbol (or any val, currently)
  (let [[[head & vals] vars] ; build compound form & collect free vars
        , (loop [expr (seq expr)  form []  vars #{}]
            (if-let [[x & expr] expr]
              (if (instance? Term x)
                (recur expr  (conj form (:form x))  (into vars (:vars x)))
                (recur expr  (conj form        x)         vars))
              [form vars]))
        [form vars] ; beta reduction
        , (if (instance? Forall head)
            (let [{:keys [form args]} head]
              (loop [args' (seq args) vals' (seq vals)  env {}]
                (if-let   [[a & rgs] args']
                  (if-let [[v & als] vals']
                    (recur rgs als (assoc env a v))
                    [(Forall. (subst env form) args') (clojure.set/difference vars (set (keys env)))])
                  (if vals'
                    (throw (ex-info "arity error" {:args args :vals vals}))
                    [(subst env form) (clojure.set/difference vars (set (keys env)))]))))
            [(cons head vals) vars])]
    (if (seq vars)
      (Term. form vars)
      (eval form))))

(comment

  (def a (!))
  (def b (!))
  (def c (!))

  (? inc 0)
  ;; 1
  (? inc a)
  ;; Term {
  ;;  :form (inc a)
  ;;  :vars #{a}
  ;; }
  (? + (inc 0) a)
  ;; Term {
  ;;   :form (+ 1 a)
  ;;   :vars #{a}
  ;; }
  (? inc (? inc a))
  ;; Term {
  ;;   :form (inc (inc a))
  ;;   :vars #{a}
  ;; }

  (def phi (? + a b))
  ;; Term {
  ;;   :form (+ a b)
  ;;   :vars #{a b}
  ;; }
  (def chi (% phi b))
  ;; Term {
  ;;   :form Forall {
  ;;           :term (+ a b)
  ;;           :args [b]
  ;;         }
  ;;   :vars #{a}
  ;; }
  (def psi (% chi a))
  ;; Term {
  ;;   :form Forall {
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

  ;; non-terminating form
  (? (% (? ? a a) a) (% (? ? a a) a))

  ;; todo test recursion

  )
