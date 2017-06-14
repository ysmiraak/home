{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-pprint "1.1.2"]
                  [lein-codox "0.10.0"]
                  [lein-kibit "0.1.2" :exclusions [org.clojure/clojure]]
                  [jonase/eastwood "0.2.4"]]}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.9.0-alpha16"]
                       [com.cemerick/pomegranate "0.3.1"]
                       [org.clojure/test.check "0.9.0"]
                       [criterium "0.4.4"]
                       [org.clojure/core.async "0.3.442"]
                       [uncomplicate/fluokitten "0.6.0"]
                       [com.rpl/specter "1.0.1"]
                       [swiss-arrows "1.0.0"]
                       [aysylu/loom "1.0.0"]
                       [prismatic/plumbing "0.5.4"]
                       [net.mikera/core.matrix "0.58.0"]
                       [net.mikera/vectorz-clj "0.46.0"]
                       [org.clojure/data.int-map "0.2.4"]
                       [org.clojure/core.logic "0.8.11"]
                       [org.clojure/core.match "0.3.0-alpha4"]
                       [clj-tuple "0.2.2"]]
        :repl-options {:init-ns ysmiraak
                       :init (do
                               (ns ysmiraak
                                 (:refer-clojure :exclude [vector hash-map])
                                 (:require [clj-tuple :refer :all]
                                           [clojure.spec :as s]
                                           [clojure.spec [gen :as g] [test :as t]]
                                           [com.rpl.specter :as ls]
                                           [swiss.arrows :refer :all]
                                           [uncomplicate.fluokitten.core :refer :all]))
                               (set! *warn-on-reflection* true)
                               (set! *print-length* 11))}}}
