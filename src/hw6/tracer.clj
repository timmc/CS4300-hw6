(ns hw6.tracer
  (:require [hw6.vec3 :as v])
  (:import (java.awt Graphics2D)
           (java.awt.image BufferedImage)))

;;; A ray is a {:start [x y z] :dir [x y z]}

(defn along-ray
  "Compute {:pt, :dist} on a ray based on the number of multiples of :dir that
are required to reach it."
  [ray t]
  (let [v (v/scale (:dir ray) t)]
    {:pt (v/sum v (:start ray)), :dist (v/mag v)}))

(defmulti intersect
  "Intersect an object and a ray. Return the first intersection as {:obj, :pt,
:dist}, or nil. "
  (fn [o r] (:type o)))
(defmethod intersect :default [_ _] nil)
(defmethod intersect :sphere [{r :radius c :center :as obj}
                              {q :start d :dir :as ray}]
  (let [q-c (v/sum q (v/scale c -1))
        A (v/dot d d)
        B (v/dot (v/scale d 2) q-c)
        C (- (v/dot q-c q-c) (* r r))
        det (- (* B B) (* 4 A C))]
    (when-not (neg? det)
      (let [t1 (/ (+ (- B) (Math/sqrt det)) (* 2 A))
            t2 (/ (- (- B) (Math/sqrt det)) (* 2 A))
            [t1 t2] (if (< t1 t2) [t1 t2] [t2 t1])
            t (when (> t2 0) (if (> t1 0) t1 t2))]
        (when t
          (assoc (along-ray ray t) :obj obj))))))

(defn ray-hits
  "Compute a seq of all object intersections in the scene with the given ray."
  [scene ray]
  (filter (complement nil?) (map #(intersect % ray) (:objects scene))))

(defn closest-hit
  "Find the closest ray hit in the scene, or nil."
  [hits]
  (if (seq hits)
    (reduce (fn closer [h1 h2] (if (< (:dist h1) (:dist h2)) h1 h2)) hits)
    nil))

(defn render
  [^Graphics2D g, scene, w, h]
  nil)

