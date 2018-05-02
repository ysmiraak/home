(defproject ysmiraak "2018.05.02"
  :description "a library of stolen ideas."
  :url "https://github.com/ysmiraak/ysmiraak"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.9.0"]
                 [swiss-arrows "1.0.0"]
                 [uncomplicate/fluokitten "0.6.1"]
                 [uncomplicate/neanderthal "0.19.0" :exclusions [org.clojure/tools.analyzer.jvm]]
                 [com.rpl/specter "1.1.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.logic "0.8.11"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/core.async "0.4.474" :exclusions [org.clojure/tools.analyzer.jvm]]
                 [criterium "0.4.4"]]
  :repl-options {:init-ns ysmiraak}
  :codox {:output-path "docs"
          :source-uri "https://github.com/ysmiraak/home/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}})
