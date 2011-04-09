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
    (is (= (intersect unit-sphere {:start [2 0 0] :dir [-1 0 0]})
           {:obj unit-sphere, :pt [1 0 0], :dist 1})) ; ...from both sides
    ))

