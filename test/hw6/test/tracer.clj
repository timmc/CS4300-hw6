(ns hw6.test.tracer
  (:use [hw6.tracer])
  (:use [clojure.test])
  (:import (java.math BigDecimal RoundingMode)))

;; From hw4 test utils
(defn cut
  "Round floating point value to specified decimal places."
  ([val] (cut 6 val))
  ([places val]
     (.. (BigDecimal. val)
         (setScale places RoundingMode/HALF_UP)
         (doubleValue))))

(deftest rays
  (is (= (along-ray {:start [100 200 300] :dir [-1 2 3]} -10)
         {:pt [110 180 270] :dist (float (Math/sqrt 1400))})))

(deftest intersect-sphere
  (let [unit-sphere {:type :sphere :center [0 0 0] :radius 1}]
    (is (= (intersect unit-sphere {:start [2 0 0] :dir [1 0 0]})
           nil)) ; no intersection behind a ray
    (is (= (intersect unit-sphere {:start [0 0 0] :dir [1 0 0]})
           {:obj unit-sphere, :pt [1 0 0], :dist 1})) ; intersect inside :-/
    (is (= (intersect unit-sphere {:start [-2 0 0] :dir [1 0 0]})
           {:obj unit-sphere, :pt [-1 0 0], :dist 1})) ; closest intersection
    (is (= (intersect unit-sphere {:start [2 0 0] :dir [-10000 0 0]})
           {:obj unit-sphere, :pt [1 0 0], :dist 1})) ; ...from both sides
    ))

(deftest ray-hit-seq
  (let [s0 {:type :sphere, :center [0 0 0], :radius 1} ; [-1 1]
        s3 {:type :sphere, :center [0 3 0], :radius 1} ; [2 4]
        s6 {:type :sphere, :center [0 6 0], :radius 1} ; [5 7]
        beads {:objects [s0 s3 s6]}]
    (is (= (ray-hits beads {:start [0 -10 0] :dir [0 1 0]})
           [{:obj s0 :pt [0 -1 0] :dist 9}
            {:obj s3 :pt [0 2 0] :dist 12}
            {:obj s6 :pt [0 5 0] :dist 15}])) ;TODO order doesn't matter
    (is (= (ray-hits beads {:start [0 1.5 0] :dir [0 1 0]})
           [{:obj s3 :pt [0 2 0] :dist 0.5}
            {:obj s6 :pt [0 5 0] :dist 3.5}])) ; filter nils
    ))

