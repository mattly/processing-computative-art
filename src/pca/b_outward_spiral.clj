(ns pca.b-outward-spiral
  (:require 
   [quil.core :as q]
   [pca.z-helpers :as h]))

(def w 1500)
(def h w)

(def cx (/ w 2))
(def cy (/ h 2))

(def size [h w])

(defn coord [r a]
  (let [[x y] (h/pol2car r a)]
    [(-> x (* cx) (+ cx))
     (-> y (* cy) (+ cy))]))

(def setup (h/setup {:static true }))

(defn fib [a b] (lazy-seq (cons a (fib b (+ a b)))))

(def fib-seq (fib 0 1))

(take 10 fib-seq)

(def top 360)
(def series
  (->> fib-seq
       (take-while #(< % top))
       (map #(/ % top))
       (partition 2 1)
       (map #(conj %2 %1) (range))))

(def angle-step q/HALF-PI)

(defn draw [_state]
  (println :drawing )
  (q/background (q/random 360) 50 90)
  (q/stroke 100)
  (q/stroke-weight 2)
  (q/no-fill)
  (doseq [[x i j] series]
    (let [xa (* x  angle-step)
          nxa (+ xa angle-step)
          [sx sy] (coord i xa)
          [ex ey] (coord j xa)]
      (q/line sx sy ex ey)
      (q/arc cx cy (* j h) (* j w) xa nxa :open))))

(q/defsketch outward-spiral
  :setup setup
  :draw draw
  :size size
  :title "outward spiral"
  :features [:no-bind-output]
  :middleware h/middleware)

(.redraw outward-spiral)
