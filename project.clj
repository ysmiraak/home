(defproject ysmiraak "2020.05.17"
  :description "a library of stolen ideas."
  :url "https://github.com/ysmiraak/ysmiraak"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[com.rpl/specter "1.1.3"]
                 [criterium "0.4.5"]
                 [org.clojure/clojure "1.10.2-alpha1"]
                 [org.clojure/core.async "1.2.603"]
                 [org.clojure/core.logic "1.0.0"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [org.clojure/test.check "1.0.0"]
                 [swiss-arrows "1.0.0"]
                 [uncomplicate/fluokitten "0.9.1"]
                 [uncomplicate/neanderthal "0.31.0"
                  :exclusions [org.jcuda/jcublas
                               org.jocl/jocl-blast
                               uncomplicate/clojurecl
                               uncomplicate/clojurecuda]]]
  :repl-options {:init-ns ysmiraak}
  :codox {:output-path "docs"
          :source-uri "https://github.com/ysmiraak/home/blob/master/{filepath}#L{line}"
          :metadata {:doc/format :markdown}})
