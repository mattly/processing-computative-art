(ns pca.b-outward-spiral
  (:require 
   [quil.core :as q]
   [pca.z-helpers :as h]))

(def w 1500)
(def h w)

(def cx (/ w 2))
(def cy (/ h 2))

(def size [h w])

(def setup (h/setup {:static true}))

(defn coord [r a]
  (let [[x y] (h/pol2car r a)]
    [(-> x (* cx) (+ cx))
     (-> y (* cy) (+ cy))]))


(def top 255)

(def series  
  (->> h/fib-seq
       (take-while #(< % top))
       (map #(/ % top))
       (partition 2 1)
       (map #(conj %2 %1) (range))))

(defn draw [_state]
  (q/background (q/random 360) 50 90)
  (q/stroke-weight 2)
  (q/no-fill)
  (doseq [n (range 4 (count series))]
    (doseq [[x i j] series]
      (let [angle-step (/ q/TWO-PI n)
            xa  (* x  angle-step)
            nxa (+ xa angle-step)
            n-rmax (/ n (count series))
            ri (/ i n-rmax)
            rj (/ j n-rmax)
            [sx sy] (coord ri xa)
            [ex ey] (coord rj xa)]
        (q/stroke (* 10 n))
        (q/line sx sy ex ey)
        (q/arc cx cy (* rj h) (* rj w) xa nxa :open)))))

(q/defsketch outward-spiral
  :setup setup
  :draw draw
  :size size
  :title "outward spiral"
  :features [:no-bind-output]
  :middleware h/middleware)

(.redraw outward-spiral)
