(ns pca.e-nesting
  (:require
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def size 1000)

(def setup (h/setup {:static true}))

(defn circlish-points [r-base r-variant]
  (let [num-points 10
        angle-segment (/ q/TWO-PI num-points)]
    (for [i (range num-points)]
      [(-> (q/random-gaussian) (q/constrain -3 3) (* r-variant) (+ r-base))
       (* i angle-segment)])))

(defn cartesianify [{:keys [i pts]}]
  (let [[y-base z-base fade]
        (cond (< i -10) [(q/map-range i -50 -10 -300 0) -10 1]
              (< i 10) [(q/map-range i -10 10 0 50)
                        (q/map-range i -10 10 -10 100)
                        (q/map-range i -10 10 1 0)]
              :else [50 (* i 10) 0])]
    (map (fn [[r a]] 
           (let [[x yz] (h/pol2car r a)]
             [x (+ y-base (* yz (- 1 fade))) (+ z-base (* yz fade))])) 
         pts)))

(defn draw-shapes [pts]
  (q/begin-shape)
  (doseq [[x y z] pts]
    (q/vertex x y z))
  (q/end-shape :close))


(defn tendril [x]
  (->> (range -50 40)
       (map (fn [i]
              {:i   i
               :pts (circlish-points 10 0.33)}))
       (map cartesianify)
       (map (partial map #(update % 0 + x)))
       (map draw-shapes)
       (doall)))

(defn draw [_state]
  (q/background 33 50 10)
  (q/camera 0 0 500
            0 0 0
            0 1 0)
  ; (q/no-stroke)
  (doseq [c (range -300 301 50)]
    (q/stroke 45 50 (if (zero? (mod c 100)) 50 25))
    (q/line c -300 0 c 300 0)
    (q/line -300 c 0 300 c 0))
  (q/no-fill)
  (q/stroke 45 100 100)
  (doseq [x (range -500 500 30)]
    (tendril x)))

(q/defsketch nesting
  :setup setup
  :draw draw
  :update identity
  :size [size size]
  :title "nesting"
  :features [:no-bind-output]
  :renderer :p3d
  :middleware h/middleware)

(.redraw nesting)
