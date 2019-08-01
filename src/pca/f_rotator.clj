(ns pca.f-rotator
  (:require
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def size 1000)

(def cx (/ size 2))
(def cy (/ size 2))

(def rmax cx)

(defn coord [[r a] & [cor coa]]
  (let [cor (or cor 0)
        coa (or coa 0)
        [xo yo] (h/pol2car cor coa)
        [x y] (h/pol2car r a)]
    [(+ cx xo x) (+ cy yo y)]))

(def primes [3 5 7 11 13 17 19 23 27 31 37 43])

(def *reset? (atom nil))
(comment (reset! *reset? true))

(defn reset-state []
  (reset! *reset? false)
  (doto 
    {:series (map (fn [i]
                    (let [size (h/qrand-nth primes)
                          hz (-> (q/random-gaussian) (* (inc i) 1/9) (q/constrain -20 20))
                          innerb (-> (q/random-gaussian) (q/constrain -5 5) (* rmax 1/4) (+ (* rmax 1/9)))
                          heightb (-> (q/random-gaussian) (q/constrain -10 10) (+ 10) (* (/ size) rmax 1/3))]
                      {:size size
                       :hz hz
                       :color (h/qrand-nth h/solarized)
                       :r/inner-base innerb
                       :r/inner-phase (-> (q/random-gaussian) (* q/PI) (mod q/PI))
                       :r/inner-factor (q/random-gaussian)
                       :r/inner-hz (* hz (q/random-gaussian) 1/11)
                       :r/height-base heightb
                       :r/height-phase (-> (q/random-gaussian) (* q/PI) (mod q/PI))
                       :r/height-factor (q/random-gaussian)
                       :r/height-hz (* hz (q/random-gaussian) 1/19)}))
                  (range (q/floor (q/random 5 7))))}
    clojure.pprint/pprint))

(def setup
  (h/setup {:setup (fn [] (reset-state))}))

(defn update-state [state]
  (if @*reset?
    (reset-state)
    state))

(defn draw [{:keys [series]}]
  (q/background 0 0 10)
  (q/stroke-cap :square)
  (let [height-max (apply max (map :r/height-base series))
        frame (/ (q/frame-count) 30)]
    (doseq [{:as s :keys [size hz color]} series]
      (doseq [n (range size)]
        (let [weight (-> (/ (:r/height-base s) height-max)
                         (q/map-range 0 1 (/ size 2) 2))
              a (-> frame (* hz) ;; frequency
                    (+ (* (/ n size) q/TWO-PI))  ;; n-phase
                    )
              inner (-> a (+ (:r/inner-phase s) (* (:r/inner-factor s))) q/sin
                        (+ (q/map-range (q/sin (* (:r/inner-hz s) frame)) -1 1 0 1))
                        (q/map-range -2 2 (:r/inner-base s) (* 2 (:r/inner-base s))))
              [sx sy] (coord [inner a])
              outer (-> a (+ (:r/height-phase s) (* (:r/height-factor s))) q/sin
                        (+ (q/map-range (q/sin (* (:r/height-hz s) frame)) -1 1 0 1))
                        (q/map-range -2 2 1 (:r/height-base s))
                        (+ inner))
              [ex ey] (coord [outer a])]
          (q/stroke-weight weight)
          (apply q/stroke color )
          (q/line sx sy ex ey))))))


(q/defsketch rotator
  :setup setup
  :draw draw
  :update update-state
  :size [size size]
  :title "rotator"
  :features [:no-bind-output]
  :middleware h/middleware)
