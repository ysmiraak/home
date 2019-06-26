(ns ysmiraak.native
  "[uncomplicate.neanderthal](http://neanderthal.uncomplicate.org/) native only."
  (:require [ysmiraak.core :refer [var-alias]]
            [uncomplicate.neanderthal core auxil random real native vect-math]))

(let [pub #(-> (str 'uncomplicate.neanderthal. %) symbol ns-publics)
      real (-> 'real pub keys set)]
  (doseq [[sym var] (pub 'core)]
    (when-not (real sym)
      (var-alias var sym)))
  (doseq [ns '[auxil random real native vect-math]
          [sym var] (pub ns)]
    (var-alias var sym)))
