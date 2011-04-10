(ns hw6.test.tracer
  (:use [hw6.tracer])
  (:use [clojure.test])
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

(deftest colors
  (is (= (rgb->int [1 0 0.2]) 0xFF0033))
  (is (= (rgb->int [1.1 0.2 -5]) 0xFF3300)))
