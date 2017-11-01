(ns ysmiraak.specter
  "[com.rpl.specter](https://github.com/nathanmarz/specter) lower-cased."
  (:refer-clojure :only [-> binding doseq ns-publics require str symbol])
  (:require [ysmiraak.core :refer [var-alias]]
            [clojure.string :as str]
            com.rpl.specter))

(doseq [[sym var] (ns-publics 'com.rpl.specter)]
  (var-alias var (-> sym str str/lower-case symbol)))
