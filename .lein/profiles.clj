{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-pprint "1.1.2"]
                  [lein-codox "0.10.0"]
                  [lein-kibit "0.1.2" :exclusions [org.clojure/clojure]]
                  [jonase/eastwood "0.2.4"]]}
 :repl {:dependencies [[com.cemerick/pomegranate "0.3.1"]
                       [ysmiraak "0.1.0-SNAPSHOT"]]
        :repl-options {:init-ns ysmiraak}
        :global-vars {*warn-on-reflection* true
                      *print-length* 11}
        :jvm-opts ["-Xmx2g"]}}
