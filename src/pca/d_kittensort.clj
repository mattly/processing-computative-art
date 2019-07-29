(ns pca.d_kittensort
  (:require 
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def size 1000)

(def setup 
  (h/setup 
   {:setup (fn []
             (let [kitten (q/load-image "src-images/1000.jpg")]
               {:kitten kitten}))
    :static true}))

(defn method-one [rows]
  (->> rows
       (map (partial sort-by (fn [p] (+ (q/brightness p) (q/saturation p)))))
       (sort-by (fn [row]
                  (->> row
                       (mapcat (juxt q/brightness q/saturation))
                       (reduce +))))
       (reverse)))

(defn method-two [rows]
  (->> rows
    (map (fn [row]
           (->> row
                (partition-by (fn [p] (q/floor (/ (q/brightness p) 15))))
                (mapcat (partial sort-by q/saturation)))))
       (partition-by (fn [row] (q/floor (/ (->> row (map q/saturation) (reduce +)) (* 5 size)))))
       (mapcat (partial sort-by (fn [row] (->> row (map q/brightness) (reduce +)))))))

(defn draw [{:keys [kitten]}]
  (q/image kitten 0 0)
  (let [px (q/pixels)]
    (println :sorting)
    (->> (range (* size size))
         (map #(aget px %))
         (partition 1000) ;; -- start rows
         (method-one) ;; first method, simple sorting
         (method-two) ;; second method, less glitchy more subtle
         (mapcat identity) ;; -- flatten rows
         (map (fn [i p] (aset-int px i p)) (range))
         (doall))
    (println :done-sorting)
    (q/update-pixels)))

(q/defsketch kittensort
  :setup setup
  :draw draw
  :update identity
  :size [size size]
  :title "kittensort"
  :features [:no-bind-output]
  :middleware h/middleware)

(.redraw kittensort)
