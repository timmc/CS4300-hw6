(ns hw6.test.vec3
  (:use [hw6.vec3])
  (:use [clojure.test]))

(deftest vectmath
  (is (= (mag [3 4 0]) 5))
  (is (= (sum [1 2 3] [10 20 30]) [11 22 33]))
  (is (= (scale [1 2 3] -1) [-1 -2 -3]))
  (is (= (dot [1 2 3] [-4 5 -6]) -12))
  (is (= (cross [3 4 0] [-4 3 0]) [0 0 25]))
  (is (= (unit [3 4 0]) [3/5 4/5 0])))
