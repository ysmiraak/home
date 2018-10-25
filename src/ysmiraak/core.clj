(ns ysmiraak.core
  "extends `clojure.core`. for example, adding some [furcula arrows](https://github.com/rplevy/swiss-arrows)."
  (:require swiss.arrows))

(def part
  "((part f a b c) x y z) = (f a b c x y z)"
  (fn [& abc]
    (fn [& xyz]
      (when-let [[f & args] (seq (concat abc xyz))]
        (apply f args)))))

(def lift
  "((lift x y z) f a b c) = (f a b c x y z)"
  (fn [& xyz]
    (fn [& abc]
      (when-let [[f & args] (seq (concat abc xyz))]
        (apply f args)))))

(def flip
  "((flip f a b c) z y x) = (f a b c x y z)"
  (fn [& abc]
    (fn [& zyx]
      (when-let [[f & args] (seq (concat abc (reverse zyx)))]
        (apply f args)))))

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
  numeric & array programming
  ```
  | lib            | namespace                       | arg        |
  |----------------+---------------------------------+------------|
  | :double        | uncomplicate.neanderthal.math   | :as d      |
  | :native        | ysmiraak.native                 | :as f      |
  | :native/linalg | uncomplicate.neanderthal.linalg | :as linalg |
  | :native*       | uncomplicate.neanderthal*       |            |
  ```
  misc
  ```
  | lib    | namespace          | arg     |
  |--------+--------------------+---------|
  | :csv   | clojure.data.csv   | :as csv |
  | :async | clojure.core.async | :as a   |
  | :bench | criterium.core     | :as b   |
  ```"
  ([lib]
   (case lib
     :spec*    (mapv library [:spec :spec/gen :spec/test])
     :logic*   (mapv library [:logic :logic/fd :logic/nom :logic/pldb])
     :native*  (mapv library [:double :native :native/linalg])
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
           ;; numeric & array
           :double        '[uncomplicate.neanderthal.math   d]
           :native        '[ysmiraak.native                 f]
           :native/linalg '[uncomplicate.neanderthal.linalg linalg]
           ;; misc
           :csv   '[clojure.data.csv   csv]
           :async '[clojure.core.async a]
           :bench '[criterium.core     b]
           ;; q w E r t
           ;; a s d f g
           ;; Z X C V b
           (throw (ex-info "`lib` unknown." {:lib lib})))
         (as-> [ns arg']
             (let [arg (or arg arg')]
               (condp apply [arg]
                 simple-symbol? [ns :as arg]
                 sequential?    (into [ns] arg)
                 (throw (ex-info "`arg` must be simple-symbol or sequential." {:arg arg})))))
         (doto require)))))
