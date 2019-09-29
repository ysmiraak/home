(ns ysmiraak.sugar)

(defmacro %
  {:style/indent 1}
  [expr & vars]
  `(fn [~@vars] ~expr))

(defn- $$
  ([] nil)
  ([form] form)
  ([f0 f1] (if (seq? f0) `(do ~f0 ~f1) f1))
  ([f0 f1 & forms]
   (if (seq? f0)
     `(do ~f0 ~(apply $$ (cons f1 forms)))
     `(let [~f0 ~f1]
        ~(apply $$ forms)))))

(defmacro $
  {:style/indent 0}
  [& forms]
  (apply $$ forms))

(comment

  ($)

  ($ 1)
  ($ (println 1))

  ($ x 1)
  ($ (println 1)
     2)

  ($ x 1
     y 2
     (+ x y))

  ($ x 1
     (println :x x)
     y 2
     (println :y y)
     (+ x y))

  ($ [x y] [1 2]
     (println :x x)
     (println :y y)
     {z :z} {:z (+ x y)}
     (println :z z)
     z)

  )
