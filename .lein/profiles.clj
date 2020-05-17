{:user {:plugins [[lein-ancient "0.6.15"]
                  [lein-pprint "1.3.2"]
                  [lein-codox "0.10.7"]]}
 :repl {:dependencies [[com.cemerick/pomegranate "1.1.0"]]
        :global-vars {*warn-on-reflection* true *print-length* 11}
        :jvm-opts ["-Xmx2g"]}}
