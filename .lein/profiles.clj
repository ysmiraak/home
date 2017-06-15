{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-pprint "1.1.2"]
                  [lein-codox "0.10.0"]
                  [lein-kibit "0.1.2" :exclusions [org.clojure/clojure]]
                  [jonase/eastwood "0.2.4"]]}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.9.0-alpha17"]
                       [clj-tuple "0.2.2"]
                       [swiss-arrows "1.0.0"]
                       [com.cemerick/pomegranate "0.3.1"]
                       [criterium "0.4.4"]
                       [org.clojure/test.check "0.9.0"]
                       [org.clojure/core.match "0.3.0-alpha4"]
                       [org.clojure/core.logic "0.8.11"]
                       [com.rpl/specter "1.0.1"]
                       [uncomplicate/fluokitten "0.6.0"]
                       [org.clojure/core.async "0.3.442"]
                       [net.mikera/core.matrix "0.58.0"]
                       [net.mikera/vectorz-clj "0.46.0"]
                       [org.clojure/data.int-map "0.2.4"]
                       [org.clojure/data.csv "0.1.4"]
                       [aysylu/loom "1.0.0"]
                       [prismatic/plumbing "0.5.4"]]
        :repl-options {:init (do
                               (set! *warn-on-reflection* true)
                               (set! *print-length* 11)
                               (ns ysmiraak
                                 "Q w E r t a s D F g z X c v"
                                 (:refer-clojure :exclude [vector hash-map])
                                 (:require [clj-tuple :refer :all]
                                           [swiss.arrows :refer :all]
                                           [clojure.spec.alpha :as s]
                                           [clojure.spec.gen.alpha :as g]
                                           [clojure.spec.test.alpha :as t]
                                           [com.rpl.specter :as w]
                                           [uncomplicate.fluokitten.core :refer :all]
                                           [clojure.core.reducers :as r]
                                           [clojure.string :as str]
                                           [clojure.set :as set]
                                           [clojure.java.io :as io]))
                               (defn library
                                 "require ns by symbols or ns-clusters by keywords:
  add-dep cemerick.pomegranate
  b criterium.core
  c clojure.core.async
  match clojure.core.match
  :logic
  v    clojure.core.logic
  fd   clojure.core.logic.fd
  nom  clojure.core.logic.nominal
  pldb clojure.core.logic.pldb
  :array
  a      clojure.core.matrix
  sel    clojure.core.matrix.selection
  linear clojure.core.matrix.linear
  rand   clojure.core.matrix.random
  stats  clojure.core.matrix.stats
  ds     clojure.core.matrix.datasets
  :data
  z   clojure.data.int-map
  csv clojure.data.csv"
                                 ([lib & libs] (mapv library (cons lib libs)))
                                 ([lib]
                                  (case lib
                                    :logic (library 'v 'fd 'nom 'pldb)
                                    :array (library 'a 'sel 'linear 'rand 'stats 'ds)
                                    :data  (library 'z 'csv)
                                    (doto
                                        (case lib
                                          add-dep '[cemerick.pomegranate :refer [add-dependencies]]
                                          b       '[criterium.core :as b]
                                          c       '[clojure.core.async :as c]
                                          match   '[clojure.core.match :refer [match]]
                                          v       '[clojure.core.logic :as v]
                                          fd      '[clojure.core.logic.fd :as fd]
                                          nom     '[clojure.core.logic.nominal :as nom]
                                          pldb    '[clojure.core.logic.pldb :as pldb]
                                          a       '[clojure.core.matrix :as a]
                                          sel     '[clojure.core.matrix.selection :as sel]
                                          linear  '[clojure.core.matrix.linear :as linear]
                                          rand    '[clojure.core.matrix.random :as rand]
                                          stats   '[clojure.core.matrix.stats :as stats]
                                          ds      '[clojure.core.matrix.dataset :as ds]
                                          z       '[clojure.data.int-map :as z]
                                          csv     '[clojure.data.csv :as csv])
                                      require
                                      (as-> [ns]
                                          (when (= 'clojure.core.matrix ns)
                                            (eval
                                             '(clojure.core.matrix/set-current-implementation
                                               :vectorz)))))))))
                       :init-ns ysmiraak}}}
