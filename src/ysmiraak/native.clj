(ns ysmiraak.native
  "[uncomplicate.neanderthal](http://neanderthal.uncomplicate.org/) native only."
  (:require [ysmiraak.core :refer [var-alias]]
            [uncomplicate.neanderthal core native real]))

(let [native (ns-publics 'uncomplicate.neanderthal.native)
      real   (ns-publics 'uncomplicate.neanderthal.real)
      core   (ns-publics 'uncomplicate.neanderthal.core)]
  (doseq [[sym var] native] (var-alias var sym))
  (doseq [[sym var] real] (var-alias var sym))
  (doseq [[sym var] core] (when-not (real sym) (var-alias var sym))))
