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

(def primes [3 5 7 11 13 17 19 23 27 31 37 41 43])

(defn gaussian-within [s bound]
  (let [n (q/random-gaussian)]
    (cond (Double/isFinite n)
          (-> n (* s) (q/constrain (- bound) bound))
          (neg? n) (- bound)
          :else bound)))

(def *reset? (atom nil))
(comment (reset! *reset? true))

(defn reset-state []
  (reset! *reset? false)
  (doto 
    {:series (map (fn [i]
                    (let [size (h/qrand-nth primes)
                          hz (gaussian-within (* (inc i) 1/9) 1)]
                      {:size  size
                       :hz    hz
                       :color (h/qrand-nth h/solarized)
                       :center {:offset    (* (q/random-gaussian) q/TWO-PI)
                                :radius    (q/random 0 (* rmax 1/31))
                                :offset/hz (gaussian-within 1/307 0.25)
                                :radius/hz (gaussian-within 1/223 0.25)}
                       :inner {:base        (+ (gaussian-within (* rmax 1/3) 5) (* rmax 1/7))
                               :phase       (-> (q/random-gaussian) (* q/TWO-PI) (mod q/TWO-PI))
                               :factor      (gaussian-within 3 13)
                               :scale       (gaussian-within 2 11)
                               :hz          (gaussian-within 1/13 1)
                               :breathe/hz  (gaussian-within 1/5 0.5)
                               :breathe/amt (q/random-gaussian)}
                       :outer {:base        (-> (gaussian-within (* (/ size) rmax 1/3) 10) (+ 10))
                               :phase       (-> (q/random-gaussian) (* q/TWO-PI) (mod q/TWO-PI))
                               :factor      (gaussian-within 2 11)
                               :scale       (gaussian-within 3 23)
                               :hz          (gaussian-within 1/37 1)
                               :breathe/hz  (gaussian-within 1/11 1)
                               :breathe/amt (q/random-gaussian)}}))
                  (range (h/qrand-nth (vec (take 7 primes)))))}
    clojure.pprint/pprint))

(def setup
  (h/setup {:setup (fn [] (reset-state))}))

(defn update-state [state]
  (cond 
    @*reset? (reset-state)
    (zero? (mod (q/frame-count) (* 30 30))) (reset-state)
    :else state))

(defn draw [{:keys [series]}]
  (apply q/background (first h/solarized))
  (q/stroke-cap :square)
  (let [height-max (->> series (map :outer) (map :base) (apply max))
        frame (/ (q/frame-count) 30)]
    (doseq [{:keys [size hz color inner outer center]} series]
      (doseq [n (range size)]
        (let [weight (-> (/ (:base outer) height-max)
                         (q/map-range 0 1 (/ size 3) 2))
              a (-> frame (* hz)
                    (+ (* (/ n size) q/TWO-PI)))
              cro (* (:radius center) 
                     (+ 1 (q/sin (* q/TWO-PI frame (:radius/hz center)))))
              cra (+ (:offset center) 
                     (* q/TWO-PI frame (:offset/hz center)))
              sr-base (+ (:base inner) #_
                         (* (:base inner)
                            (:breathe/amt inner)
                            (q/sin (* q/TWO-PI frame (:breathe/hz inner)))))
              sr (-> ; (* (:hz inner) frame q/TWO-PI)
                     (+ a (:phase inner))
                     q/sin
                     (q/map-range -1 1 0.00001 0.99999)
                    ;  (q/pow (:factor inner))
                    ;  (* (:scale inner))
                     (q/map-range 0 1 sr-base (* 2 sr-base)))
              [sx sy] (coord [sr a] cro cra)
              er-end (+ (:base outer) #_
                        (* (:base outer)
                           (:breathe/amt outer)
                           (+ 1 (q/sin (* q/TWO-PI frame (:breathe/hz outer))))))
              er (-> ; (* (:hz outer) frame q/TWO-PI)
                     (+ a (:phase outer))
                     q/sin
                     (q/map-range -1 1 0.00001 0.99999)
                    ;  (q/constrain 0.00001 1)
                    ;  (q/pow (:factor outer))
                    ;  (* (:scale outer))
                     (q/map-range 0 1 5 er-end))
              [ex ey] (coord [(+ sr er) a] cro cra)]
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
