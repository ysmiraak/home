{:user {:plugins [[lein-ancient "0.6.12"]
                  [lein-pprint "1.1.2"]
                  [lein-codox "0.10.3"]
                  [lein-kibit "0.1.5" :exclusions [org.clojure/clojure]]
                  [jonase/eastwood "0.2.5"]]}
 :repl {:dependencies [^:displace [org.clojure/clojure "1.9.0-beta2"]
                       [ysmiraak "2017.10.16"]
                       [com.cemerick/pomegranate "0.4.0"]]
        :repl-options {:init-ns ysmiraak}
        :global-vars {*warn-on-reflection* true
                      *print-length* 11}
        :jvm-opts ["-Xmx2g"]}}
