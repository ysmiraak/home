(defproject ysmiraak "0.0.0"
  :description "master project."
  :url "https://github.com/ysmiraak/home-backup"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [uncomplicate/fluokitten "0.5.1"]
                 [lonocloud/synthread "1.4.0"]
                 [prismatic/plumbing "0.5.3"]
                 [net.mikera/core.matrix "0.57.0"]
                 [net.mikera/vectorz-clj "0.45.0"]
                 [org.clojure/data.int-map "0.2.4"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [clj-tuple "0.2.2"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}})
