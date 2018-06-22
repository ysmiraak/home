(ns ysmiraak.core
  "extends `clojure.core`. for example, adding some [furcula arrows](https://github.com/rplevy/swiss-arrows)."
  (:require swiss.arrows))

(defn invoke
  "calls `f` with given arguments."
  ([^clojure.lang.AFn f] (.invoke f))
  ([^clojure.lang.AFn f x] (.invoke f x))
  ([^clojure.lang.AFn f x y] (.invoke f x y))
  ([^clojure.lang.AFn f x y z] (.invoke f x y z))
  ([f x y z & args] (apply f x y z args)))

(defn flip
  "reverses the order of arguments for function `f`."
  ([f]
   (fn ([] (f))
     ([x] (f x))
     ([x y] (f y x))
     ([x y z] (f z y x))
     ([x y z & args] (apply f (rseq (into [x y z] args))))))
  ([f x]
   (fn ([] (f x))
     ([y] (f y x))
     ([y z] (f z y x))
     ([y z & args] (apply f (rseq (into [x y z] args))))))
  ([f x y]
   (fn ([] (f y x))
     ([z] (f z y x))
     ([z & args] (apply f (rseq (into [x y z] args))))))
  ([f x y z]
   (fn ([] (f z y x))
     ([& args] (apply f (rseq (into [x y z] args))))))
  ([f x y z & args]
   (fn [& args'] (apply f (rseq (into (into [x y z] args) args'))))))

(defn queue
  "creates a new queue containing the `items`."
  [& items]
  (into clojure.lang.PersistentQueue/EMPTY items))

(defn var-alias
  "defines `sym` in the current namespace with the value and meta of `var`."
  [var sym]
  (doto (eval `(def ~sym (var-get ~var)))
    (reset-meta! (assoc (meta var) :name sym :ns *ns*))))

(var-alias #'swiss.arrows/-< '-<)
(var-alias #'swiss.arrows/-<:p '-<=)
(var-alias #'swiss.arrows/-<< '-<<)
(var-alias #'swiss.arrows/-<<:p '-<<=)

(defn library
  "requires namespace by `lib` with `arg`.
  ```
  | lib        | namespace               | arg                 |
  |------------+-------------------------+---------------------|
  | :spec      | clojure.spec.alpha      | :as s               |
  | :spec/gen  | clojure.spec.gen.alpha  | :as g               |
  | :spec/test | clojure.spec.test.alpha | :as t               |
  | :spec*     | clojure.spec*           |                     |
  | :test      | clojure.test            | :as test            |
  | :string    | clojure.string          | :as str             |
  | :edn       | clojure.edn             | :as edn             |
  | :instant   | clojure.instant         | :as inst            |
  | :pprint    | clojure.pprint          | :as pp              |
  | :inspector | clojure.inspector       | :as insp            |
  | :repl      | clojure.repl            | :as repl            |
  | :javadoc   | clojure.java.javadoc    | :refer [javadoc]    |
  | :browse    | clojure.java.browse     | :refer [browse-url] |
  | :shell     | clojure.java.shell      | :as sh              |
  | :io        | clojure.java.io         | :as io              |
  | :set       | clojure.set             | :as set             |
  | :reducers  | clojure.core.reducers   | :as r               |
  | :clojure*  | clojure*                |                     |
  ```
  category & declarative programming
  ```
  | lib         | namespace                    | arg            |
  |-------------+------------------------------+----------------|
  | :fluokitten | uncomplicate.fluokitten.core | :refer :all    |
  | :SPECTER    | com.rpl.specter              | :as w          |
  | :specter    | ysmiraak.specter             | :as w          |
  | :match      | clojure.core.match           | :refer [match] |
  | :logic      | clojure.core.logic           | :as q          |
  | :logic/fd   | clojure.core.logic.fd        | :as fd         |
  | :logic/nom  | clojure.core.logic.nominal   | :as nom        |
  | :logic/pldb | clojure.core.logic.pldb      | :as pldb       |
  | :logic*     | clojure.core.logic*          |                |
  ```
  data & array programming
  ```
  | lib            | namespace                          | arg        |
  |----------------+------------------------------------+------------|
  | :double        | uncomplicate.neanderthal.math      | :as d      |
  | :native        | ysmiraak.native                    | :as f      |
  | :native/vect   | uncomplicate.neanderthal.vect-math | :as v      |
  | :native/linalg | uncomplicate.neanderthal.linalg    | :as linalg |
  | :native*       | uncomplicate.neanderthal*          |            |
  | :csv           | clojure.data.csv                   | :as csv    |
  ```
  misc
  ```
  | lib        | namespace          | arg   |
  |------------+--------------------+-------|
  | :async     | clojure.core.async | :as c |
  | :criterium | criterium.core     | :as b |
  ```"
  ([lib]
   (case lib
     :spec*    (mapv library [:spec :spec/gen :spec/test])
     :logic*   (mapv library [:logic :logic/fd :logic/nom :logic/pldb])
     :native*  (mapv library [:double :native :native/vect :native/linalg])
     :clojure* (mapv library [:spec* :test :string :edn :instant :repl :pprint :inspector :javadoc :browse :shell :io :set :reducers])
     (library lib nil)))
  ([lib arg]
   (binding [*warn-on-reflection* false]
     (-> (case lib
           :spec      '[clojure.spec.alpha      s]
           :spec/gen  '[clojure.spec.gen.alpha  g]
           :spec/test '[clojure.spec.test.alpha t]
           :test      '[clojure.test            test]
           :string    '[clojure.string          str]
           :edn       '[clojure.edn             edn]
           :instant   '[clojure.instant         inst]
           :pprint    '[clojure.pprint          pp]
           :inspector '[clojure.inspector       insp]
           :repl      '[clojure.repl            repl]
           :javadoc   '[clojure.java.javadoc    [:refer [javadoc]]]
           :browse    '[clojure.java.browse     [:refer [browse-url]]]
           :shell     '[clojure.java.shell      sh]
           :io        '[clojure.java.io         io]
           :set       '[clojure.set             set]
           :reducers  '[clojure.core.reducers   r]
           ;; category & declarative
           :fluokitten '[uncomplicate.fluokitten.core [:refer :all]]
           :SPECTER    '[com.rpl.specter              w]
           :specter    '[ysmiraak.specter             w]
           :match      '[clojure.core.match           [:refer [match]]]
           :logic      '[clojure.core.logic           q]
           :logic/fd   '[clojure.core.logic.fd        fd]
           :logic/nom  '[clojure.core.logic.nominal   nom]
           :logic/pldb '[clojure.core.logic.pldb      pldb]
           ;; array & numeric
           :double           '[uncomplicate.neanderthal.math      d]
           :native           '[ysmiraak.native                    f]
           :native/vect      '[uncomplicate.neanderthal.vect-math v]
           :native/linalg    '[uncomplicate.neanderthal.linalg    linalg]
           :csv              '[clojure.data.csv csv]
           ;; misc
           :async '[clojure.core.async c]
           :criterium '[criterium.core b]
           ;; q w E r t, A s d f g, Z X c v b
           (throw (ex-info "`lib` unknown." {:lib lib})))
         (as-> [ns arg']
             (let [arg (or arg arg')]
               (condp apply [arg]
                 simple-symbol? [ns :as arg]
                 sequential?    (into [ns] arg)
                 (throw (ex-info "`arg` must be simple-symbol or sequential." {:arg arg})))))
         (doto require)))))
