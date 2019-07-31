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

(defn car2pol [[x y]]
  "given coordinates x and y with 0,0 as center of circle, returns radius and angle in radians"
  [(q/sqrt (+ (q/sq x) (q/sq y)))
   (let [xx (cond (pos? x) :pos (neg? x) :neg (zero? x) :zero)
         yy (cond (pos? y) :pos (neg? y) :neg (zero? y) :zero)]
     (case [xx yy]
       [:zero :zero] 0
       [:zero :pos] q/HALF-PI
       [:zero :neg] (* q/PI 1.5)
       ([:pos :pos] [:pos :zero]) (q/atan (/ y x))
       [:pos :neg] (+ (* q/PI 2) (q/atan (/ y x)))
       (+ q/PI (q/atan (/ y x)))))])
