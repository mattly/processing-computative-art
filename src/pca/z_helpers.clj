(ns pca.z-helpers
  (:require [quil.core :as q]
            [quil.middleware :as q.mw]))

(def middleware 
  [q.mw/fun-mode
   #_
   q.mw/pause-on-error])

(defn setup [opts]
  (fn []
    (println :setup)
    (q/color-mode :hsb 360 100 100 1.0)
    (when (:static opts)
      (q/frame-rate 1)
      (q/no-loop))
    (if-let [f (:setup opts)]
      (f)
      {})))


(defn- fib [a b] (lazy-seq (cons a (fib b (+ a b)))))

(def fib-seq (fib 0 1))


(defn pol2car 
  "given radius r and angle a in radians, returns cartesian offset"
  [r a]
  [(* r (q/cos a))
   (* r (q/sin a))])
