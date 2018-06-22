(ns ysmiraak.native
  "[uncomplicate.neanderthal](http://neanderthal.uncomplicate.org/) native only."
  (:require [ysmiraak.core :refer [var-alias]]
            [uncomplicate.neanderthal core aux real native]))

(let [core   (ns-publics 'uncomplicate.neanderthal.core)
      aux    (ns-publics 'uncomplicate.neanderthal.aux)
      real   (ns-publics 'uncomplicate.neanderthal.real)
      native (ns-publics 'uncomplicate.neanderthal.native)]
  (doseq [[sym var] core] (when-not (real sym) (var-alias var sym)))
  (doseq [ns [aux real native] [sym var] ns] (var-alias var sym)))
