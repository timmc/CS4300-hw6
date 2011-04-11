(ns hw6.test.vec3
  (:use [hw6.vec3])
  (:use [clojure.test]))

(deftest vectmath
  (is (= (mag [3 4 0]) 5))
  (is (= (sum) [0 0 0]))
  (is (= (sum [1 2 3]) [1 2 3]))
  (is (= (sum [1 2 3] [10 20 30]) [11 22 33]))
  (is (= (sum [1 2 3] [10 20 30] [100 200 300]) [111 222 333]))
  (is (= (scale [1 2 3] 2) [2 4 6]))
  (is (= (neg [1 2 3]) [-1 -2 -3]))
  (is (= (diff [10 20 30] [1 2 3]) [9 18 27]))
  (is (= (<-pts [1 2 3] [10 20 30]) [9 18 27]))
  (is (= (dot [1 2 3] [-4 5 -6]) -12))
  (is (= (cross [3 4 0] [-4 3 0]) [0 0 25]))
  (is (= (elop * [0 11 100] [1 2 3]) [0 22 300]))
  (is (= (avg [2 4 6] [0 9 5] [1 11 -2]) [1 8 3]))
  (is (= (unit [3 4 0]) [3/5 4/5 0])))
