(ns pca.d_kittensort
  (:require 
   [pca.z-helpers :as h]
   [quil.core :as q]))

(def size 1000)
(def image-width 650)
(def image-height 1000)

(def setup 
  (h/setup 
   {:setup (fn []
             (let [kitten (q/load-image "etc/650.jpg")]
               {:kitten kitten}))
    :static true}))

(defn method-one [rows]
  (->> rows
       (map (partial sort-by (fn [p] (+ (* 2 (q/brightness p)) (q/saturation p)))))
       (map (fn [row]
              (->> row
                   (take (- size image-width))
                   (sort-by q/brightness)
                   (apply conj row))))
       (sort-by (fn [row]
                  (->> row
                       (mapcat (juxt q/brightness q/saturation))
                       (reduce +))))
       (reverse)))

(defn method-two [rows]
  (let [chunk-size (q/floor (q/random 42 166))
        chunk-count (q/ceil (/ image-width chunk-size))
        makeup-width (/ (- size image-width) chunk-count)]
    (->> rows
         (map (fn [row]
                (->> row
                     (partition-by (fn parition-chunks [p] (q/floor (/ (q/brightness p) 15))))
                     (mapcat (partial sort-by q/saturation))
                    ;  #_
                     (partition-all chunk-size)
                    ;  #_
                     (mapcat (fn makeup-chunks [chunk]
                               (->> chunk
                                    (sort-by q/brightness)
                                    (take makeup-width)
                                    (apply conj chunk)))))))
         (partition-by (fn [row] (q/floor (/ (->> row (map q/saturation) (reduce +)) (* 5 size)))))
         (mapcat (partial sort-by (fn [row] (->> row (map q/brightness) (reduce +))))))))

(defn draw [{:keys [kitten]}]
  (let [canvas  (q/pixels)
        kpixels (q/pixels kitten)]
    (println :sorting)
    (time
     (->> kpixels
       (partition image-width) ;; -- start rows
       (method-one) ;; first method, simple sorting
      ;  (method-two) ;; second method, less glitchy more subtle
       (mapcat identity) ;; -- flatten rows
       (map (fn [i p] (aset-int canvas i p)) (range))
       (doall)))
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
