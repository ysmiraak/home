(defproject ysmiraak "0.0.0"
  :description "master project."
  :url "https://github.com/ysmiraak/home-backup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/core.async "0.3.442"]
                 [uncomplicate/fluokitten "0.6.0"]
                 [lonocloud/synthread "1.4.0"]
                 [swiss-arrows "1.0.0"]
                 [aysylu/loom "1.0.0"]
                 [prismatic/plumbing "0.5.4"]
                 [net.mikera/core.matrix "0.58.0"]
                 [net.mikera/vectorz-clj "0.46.0"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [clj-tuple "0.2.2"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
