{:user {:plugins [[lein-ancient "1.0.0-RC4-SNAPSHOT"]
                  [lein-pprint "1.3.2"]
                  [lein-codox "0.10.8"]]}
 :repl {:plugins [[cider/cider-nrepl "0.29.0"]
                  [refactor-nrepl "3.6.0"]]
        :dependencies [[com.cemerick/pomegranate "1.1.0"]]
        :global-vars {*warn-on-reflection* true *print-length* 11}
        :jvm-opts ["-Xmx2g"]}}
