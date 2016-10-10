{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-pprint "1.1.2"]
                  [lein-codox "0.10.0"]
                  [lein-kibit "0.1.2"]
                  [jonase/eastwood "0.2.3"]]
        :dependencies [[com.cemerick/pomegranate "0.3.1"]]
        :repl-options {:init (do (set! *print-length* 11)
                                 (set! *warn-on-reflection* true))}}}
