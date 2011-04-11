(ns hw6.test.tracer
  (:use [hw6.tracer])
  (:use [clojure.test])
  (:require [hw6.parser :as p])
  (:require [hw6.vec3 :as v])
  (:import (java.math BigDecimal RoundingMode)))

;; From hw4 test utils
(defn cut
  "Round floating point value to specified decimal places."
  ([^Double val] (cut 6 val))
  ([places, ^Double val]
     (.. (BigDecimal. val)
         (setScale places RoundingMode/HALF_UP)
         (doubleValue))))

(deftest rays
  (is (= (along-ray {:start [100 200 300] :dir [-1 2 3] :bounces 0} -10)
         {:pt [110 180 270] :dist (float (Math/sqrt 1400))})))

(deftest intersect-sphere
  (let [unit-sphere {:type :sphere :center [0 0 0] :radius 1}]
    ;; no intersection behind a ray
    (is (= (intersect unit-sphere {:start [2 0 0] :dir [1 0 0] :bounces 0})
           nil))
    ;; intersect inside :-/
    (is (= (intersect unit-sphere {:start [0 0 0] :dir [1 0 0] :bounces 0})
           {:obj unit-sphere :pt [1 0 0] :dist 1 :normal [1 0 0]}))
    ;; closest intersection
    (is (= (intersect unit-sphere {:start [-2 0 0] :dir [1 0 0] :bounces 0})
           {:obj unit-sphere :pt [-1 0 0] :dist 1 :normal [-1 0 0]}))
    ;; ...from both sides
    (is (= (intersect unit-sphere {:start [2 0 0] :dir [-10000 0 0] :bounces 0})
           {:obj unit-sphere :pt [1 0 0] :dist 1 :normal [1 0 0]}))
    ;; glancing hit
    (let [glancing (intersect unit-sphere
                              {:start [3/5 10 0] :dir [0 -1 0] :bounces 0})]
      (is (= (map cut (:pt glancing)) [3/5 4/5 0]))
      (is (= (map cut (:normal glancing)) [3/5 4/5 0]))
      (is (= (cut (:dist glancing)) (- 10 4/5)))))
  (let [sphere2345 {:type :sphere :center [3 4 5] :radius 2}]
    ;; displaced origin
    (is (= (intersect sphere2345 {:start [3 4 10] :dir [0 0 -5] :bounces 0})
           {:obj sphere2345 :pt [3 4 7] :dist 3 :normal [0 0 1]}))))

(deftest intersect-plane
  (let [plane5 {:type :plane :pt [30 40 5] :normal [0 0 5]}]
    ;; from below...
    (is (= (intersect plane5 {:start [0 0 0] :dir [0 0 1]})
           {:obj plane5 :pt [0 0 5] :dist 5 :normal [0 0 -1]}))
    ;; ...and above (notice flipped normal)
    (is (= (intersect plane5 {:start [0 0 10] :dir [0 0 -1]})
           {:obj plane5 :pt [0 0 5] :dist 5 :normal [0 0 1]}))
    ;; no negative intersect
    (is (= (intersect plane5 {:start [0 0 0] :dir [0 0 -1]})
           nil))
    ;; no parallel intersect
    (is (= (intersect plane5 {:start [0 0 0] :dir [1 13 0]})
           nil)))
  (let [plane210 {:type :plane :pt [1 3 0] :normal [2 1 0]}
        inter-x (intersect plane210 {:start [0 0 0] :dir [100 0 0]})]
    (is (= (map cut (:pt inter-x)) [2.5 0 0]))
    (is (= (cut (:dist inter-x)) 2.5))
    (is (= (map cut (:normal inter-x))
           (map cut (v/unit [-2 -1 0]))))))

(deftest ray-vs-scene
  (let [s0 {:type :sphere, :center [0 0 0], :radius 1} ; [-1 1]
        s3 {:type :sphere, :center [0 3 0], :radius 1} ; [2 4]
        s6 {:type :sphere, :center [0 6 0], :radius 1} ; [5 7]
        beads {:objects [s0 s3 s6]}]
    ;; TODO order doesn't matter
    (is (= (ray-hits beads {:start [0 -10 0] :dir [0 1 0] :bounces 0})
           [{:obj s0 :pt [0 -1 0] :dist 9 :normal [0 -1 0]}
            {:obj s3 :pt [0 2 0] :dist 12 :normal [0 -1 0]}
            {:obj s6 :pt [0 5 0] :dist 15 :normal [0 -1 0]}]))
    ;; filter nils
    (is (= (ray-hits beads {:start [0 1.5 0] :dir [0 1 0] :bounces 0})
           [{:obj s3 :pt [0 2 0] :dist 0.5 :normal [0 -1 0]}
            {:obj s6 :pt [0 5 0] :dist 3.5 :normal [0 -1 0]}]))
    ;; from one direction...
    (is (= (closest-hit (ray-hits beads
                                  {:start [0 -10 0] :dir [0 1 0] :bounces 0}))
           {:obj s0 :pt [0 -1 0] :dist 9 :normal [0 -1 0]}))
    ;; ...and the other.
    (is (= (closest-hit (ray-hits beads
                                  {:start [0 10 0] :dir [0 -20 0] :bounces 0}))
           {:obj s6 :pt [0 7 0] :dist 3 :normal [0 1 0]}))))

(deftest cam-coords
  (binding [*camera-fov* 90]
    ;; middle
    (is (= (map cut (pixel->cam-coord 3 5 1 2))
           (map cut [0 0 (- 1/2)])))
    ;; upper left
    (is (= (map cut (pixel->cam-coord 3 5 0 0))
           (map cut [(- 1/3) (* 2/5 5/3) (- 1/2)])))))

(deftest view-rays
  (binding [*camera-fov* 90]
    (let [camera (p/expand-camera {:pose {:start [10 0 0] :dir [0 0 -100]}} nil)
          rays (image-rays camera 3 5)
          {pixel00 :pixel
           {start00 :start
            dir00 :dir
            bounces00 :bounces} :ray} (first rays)
          {pixel21 :pixel ;; name assumes y x order, but test doesn't
           {start21 :start
            dir21 :dir
            bounces21 :bounces} :ray} (nth rays 7)]
      (is (= (count rays) 15))
      ;; upper-left
      (is (= pixel00 [0 0]))
      (is (= start00) [10 0 0])
      (is (= (map cut dir00)
             (map cut [(- 1/3) (* 2/5 5/3) (- 1/2)])))
      (is (= bounces00 0))
      ;; middle
      (is (= pixel21 [1 2]))
      (is (= start21) [10 0 0])
      (is (= (map cut dir21)
             (map cut [0 0 (- 1/2)])))
      (is (= bounces21 0)))))

(deftest colors
  (is (= (rgb->int [1 0 0.2]) 0xFF0033))
  (is (= (rgb->int [1.1 0.2 -5]) 0xFF3300)))

(deftest light-dir
  (is (= (light-to {:type :point :source [103 504 0]}
                   [100 500 0])
         [(- 3/5) (- 4/5) 0]))
  (is (= (light-to {:type :directional :direction [3 0 4]}
                   [100 200 300])
         [3/5 0 4/5])))

