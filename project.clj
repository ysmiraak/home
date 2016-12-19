(defproject ysmiraak "0.0.0"
  :description "master project."
  :url "https://github.com/ysmiraak/home-backup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [clj-tuple "0.2.2"]

                 ;; [org.clojure/core.match "0.3.0-alpha4"]
                 ;; [org.clojure/core.logic "0.8.11"]
                 ;; [org.clojure/algo.monads "0.1.6"]
                 ;; [org.clojure/tools.macro "0.1.5"]
                 
                 ;; [org.clojure/math.numeric-tower "0.0.4"]
                 ;; [org.clojure/math.combinatorics "0.1.3"]

                 ;; [net.mikera/core.matrix "0.55.0"]
                 ;; [net.mikera/vectorz-clj "0.45.0"]
                 ;; [org.clojure/data.xml "0.0.8"]
                 ;; [org.clojure/data.csv "0.1.3"]

                 ]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
