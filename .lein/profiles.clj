{:user {:plugins [[lein-ancient "0.6.15"]
                  [lein-pprint "1.2.0"]
                  [lein-kibit "0.1.6" :exclusions [org.clojure/clojure]]
                  [lein-codox "0.10.4"]]}
 :repl {:dependencies [[com.cemerick/pomegranate "1.0.0"]]
        :global-vars {*warn-on-reflection* true}
        :jvm-opts ["-Xmx2g"]}}
