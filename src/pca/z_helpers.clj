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
    (if (:static opts)
      (do 
        (q/frame-rate 1)
        (q/no-loop))
      (do
        (q/frame-rate 30)))
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

(defn normalize-hsb [[h s b]] [(* h 360) (* s 100) (* 100 b)])

(def solarized
  (->> [[0.53 1.00 0.21]
        [0.53 0.90 0.26]
        [0.54 0.25 0.46]
        [0.53 0.15 0.57]
        [0.52 0.13 0.59]
        [0.50 0.09 0.63]
        [0.13 0.11 0.93]
        [0.12 0.10 0.99]
        [0.13 1.00 0.71]
        [0.05 0.90 0.80]
        [0.00 0.75 0.86]
        [0.92 0.75 0.83]
        [0.66 0.45 0.77]
        [0.57 0.82 0.82]
        [0.49 0.74 0.63]
        [0.19 1.00 0.60]]
       (mapv normalize-hsb)))

(defn qrand-nth [coll]
  (get coll (q/floor (q/random (count coll)))))
