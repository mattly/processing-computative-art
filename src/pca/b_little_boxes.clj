(ns pca.b-little-boxes
  (:require 
   [quil.core :as q]
   [pca.z-helpers :as h]))

(def width 1000)

(def setup (h/setup {:static true}))

(defn draw [_state]
  (q/background 0 0 0)
  (q/no-stroke)
  (loop [grid-size (q/floor (q/random 6 36))
         fill      20]
    (q/fill 0 0 fill 0.5)
    (let [half-step  (/ 1 grid-size 2)]
      (doseq [x (range grid-size)
                y (range grid-size)]
          (let [rel-size  (q/random-gaussian)
                half-size (* rel-size half-step width)
                cx        (-> x (* half-step 2) (+ half-step) (* width))
                cy        (-> y (* half-step 2) (+ half-step) (* width))
                inner-size (q/random 0.5 0.9)]
            (when (< 10 half-size)
              (q/begin-shape)
              (doseq [[xm ym] [[1 1] [1 -1] [-1 -1] [-1 1]]]
                (q/vertex (+ cx (* half-size xm)) (+ cy (* half-size ym))))
              (q/begin-contour)
              (doseq [[xm ym] [[1 1] [-1 1] [-1 -1] [1 -1]]]
                (q/vertex (+ cx (* half-size xm inner-size))
                          (+ cy (* half-size ym inner-size))))
              (q/end-contour)
              (q/end-shape :close)))))
    (let [next-grid (- grid-size (q/floor (q/abs (* 3 (q/random-gaussian)))))]
      (when (pos? next-grid)
        (recur next-grid (/ (+ 100 fill) 2)))))
  (println :done))

(q/defsketch little-boxes
  :setup setup
  :draw draw
  :size [width width]
  :title "little-boxes"
  :features [:no-bind-output]
  :renderer :p2d
  :middleware h/middleware)

(.redraw little-boxes)
