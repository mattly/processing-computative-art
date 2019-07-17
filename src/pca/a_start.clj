(ns pca.a-start
  (:require
   [quil.core :as q]))

(defn draw []
  (q/background 200)
  (q/stroke 0)
  (q/fill 0)
  (doseq [a (->> (range -180 180) (map #(/ % (/ Math/PI -2))))]
    (q/rect (+ 250 (* 220 (q/cos a)))
            (+ 250 (* 220 (q/sin a)))
            3 3)))

(comment
  (q/defsketch
    a-start
    :title "starting somewhere"
    :draw draw
    :size [500 500]))
