(ns pca.e-nesting
  (:require
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def size 1000)

(def setup 
  (h/setup 
   {:setup (fn []
             (let [s (dosync (q/load-shader "etc/toon-frag.glsl" "etc/toon-vert.glsl"))]
               (println s)
               {:toon s}))
    :static true}))

(defn draw [state]
  (println state)
  (println :start)
  (time
   (let [n             200
         angle-segment (/ q/TWO-PI n)
         z-max         500]
     (q/reset-shader)
     (q/background 0 0 10)
     (q/camera 0 40 0
               0 -50 500
               0 1 0)
     (q/directional-light 0 0 200 0 0 -1)
     (q/directional-light 0 0 66 1 0.5 0)
     (q/light-falloff 3 0.03 0)
    ;  #_
     (doseq [z (map #(* % (/ z-max 4)) (range 4))]
         (q/point-light 0 0 98 0 0 z))
     (q/ambient-light 0 0 95)
    ;  (q/shader (:toon state))
     (q/no-stroke)
    ;  (q/stroke 0 0 0)

     (doseq [i (range n)]
       (let [hz     (* 10 (/ (- 1 (q/random 1))))
             r-base (-> (q/random-gaussian) (q/constrain -3 3) (* 10) (+ 60))
             hue    (q/degrees (* i angle-segment))]
         (loop [z 10]
           (let [a        (mod (+ (* i angle-segment) (* q/QUARTER-PI (q/random-gaussian))) q/TWO-PI)
                 strength (-> z (/ z-max) (* 2) (q/constrain 1 2) (- 1) (q/pow 2) (* -1) (+ 1))
                 r        (-> (q/noise z)
                              (q/constrain (/ 20 z) 1)
                              (* 10)
                              (+ (* strength r-base)))
                 [x y]    (h/pol2car r a)
                 sat      (q/map-range (* (q/sin (q/pow z 0.33)) (q/sin z)) -1 1 0 100)
                 bri      (-> z
                              (q/pow 0.5)
                              (q/sin)
                              (q/map-range -1 1 0 100))]
             (q/fill hue sat bri)
             (q/with-translation [x y z]
               (q/with-rotation [q/HALF-PI 0 -1 0]
                 (q/with-rotation [(+ a q/PI) 1 0 0]
                   (q/box 10 10 (* 0.5 hz (q/noise x y z))))))
             (if (< z z-max)
               (recur (+ z hz))))))))))

(q/defsketch descent
  :setup setup
  :draw draw
  :update identity
  :size [size size]
  :title "descent"
  :features [:no-bind-output]
  :renderer :p3d
  :middleware h/middleware)

(.redraw descent)
