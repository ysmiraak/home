(ns ysmiraak.repl)

(defn del
  "unmaps `syms` from the current namespace."
  [& syms] (doseq [sym syms] (ns-unmap *ns* sym)))

(defn clear
  "unmaps all interned vars from `ns` which defaults to the current namespace."
  ([] (clear *ns*))
  ([ns] (->> ns ns-interns keys (apply del))
   (System/gc)))
