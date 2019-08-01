(ns pca.c_boxing_in
  (:require
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def w 1000)
(def h w)

(def size [h w])

(def setup (h/setup {:static true}))

(def bg 230)

(defn local-pol2car [lx ly r a]
  (let [[x y] (h/pol2car r a)]
    [(+ lx x) (+ ly y)]))

(defn on-grid [p step]
  (-> p (* step) (+ (/ step 2) (* -2 step))))

(defn draw [_state]
  (q/clear)
  (q/background bg)
  (q/fill 128)
  (q/no-stroke)
  (let [n 50
        step (* (/ 1 n))
        series (->> h/fib-seq
                    (drop 1)
                    (take (+ n 4))
                    (map list
                         (range -2 (+ n 2))))]
    (doseq [[xn x] series
            [yn y] series]
      (let [lcx     (-> xn
                    ;  #_
                        (+ (* (/ yn n)
                              (/ x y y (inc (q/abs xn)))))
                    ; #_
                        (+ (* (q/pow (/ yn n) 2)
                              (q/sin x)))
                        (on-grid step)
                        (* w))
            lcy     (-> yn
                    ; #_
                        (+ (* (/ (- n yn) n)
                              (q/cos x)))
                      ; #_
                        (+ (* (/ yn n)
                              (/ y x (inc (q/pow yn 2)))))
                        (on-grid step)
                        (* h))
            lrotate (* (/ yn n) (/ yn n) (q/sin x) (q/cos y))
            fill    (let [g      (-> n (- yn) (/ n)
                                     (+ (* 0.25 (q/sin x)))
                                     (* 330)
                                     (+ 15))
                          thresh 60
                          dist   (- g bg)]
                      (cond (< (* -1 thresh) dist 0) (- bg thresh)
                            (< 0 dist thresh) (+ bg thresh)
                            :else g))
            rad-tr  (* step w (+ (q/pow (/ yn n) (/ 2)) (* 0.25 (q/cos x))))
            rad-br  (* step w (+ (q/pow (/ yn n) (/ 2)) (* 0.25 (q/sin y))))
            rad-bl  (* step w (+ (q/pow (/ yn n) (/ 2)) (* 0.25 (q/sin x))))
            rad-tl  (* step w (+ (q/pow (/ yn n) (/ 2)) (* 0.25 (q/cos x))))
            coords  (mapcat #(local-pol2car lcx lcy %1 %2)
                            [rad-tr rad-br rad-bl rad-tl]
                            (map #(-> %
                                      (/ 4)
                                      (+ lrotate)
                                      (* q/TWO-PI)
                                      (+ q/QUARTER-PI))
                                 (range 4)))]
        (q/with-fill (float fill)
          (apply q/quad coords))))))

(q/defsketch boxing-in
  :setup setup
  :draw draw
  :size size
  :title "boxing in"
  :features [:no-bind-output]
  :middleware h/middleware)

(.redraw boxing-in)
