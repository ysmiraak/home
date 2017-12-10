{:user {:plugins [[lein-ancient "0.6.14"]
                  [lein-pprint "1.2.0"]
                  [lein-codox "0.10.3"]
                  [lein-kibit "0.1.6" :exclusions [org.clojure/clojure]]
                  [jonase/eastwood "0.2.5"]]}
 :repl {:dependencies [[com.cemerick/pomegranate "1.0.0"]]
        :global-vars {*warn-on-reflection* true
                      *print-length* 11}
        :jvm-opts ["-Xmx2g"]}}
