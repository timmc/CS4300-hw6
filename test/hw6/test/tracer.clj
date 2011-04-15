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
  (let [sphere1 {:type :sphere :center [0 0 0] :radius 1}]
    ;; no intersection behind a ray
    (is (= (intersect sphere1 {:start [2 0 0] :dir [1 0 0] :bounces 0})
           nil))
    ;; intersect inside :-/
    (let [ray {:start [0 0 0] :dir [1 0 0] :bounces 0}]
      (is (= (intersect sphere1 ray)
             {:obj sphere1 :pt [1 0 0] :dist 1 :normal [1 0 0] :ray ray})))
    ;; closest intersection
    (let [ray {:start [-2 0 0] :dir [1 0 0] :bounces 0}]
      (is (= (intersect sphere1 ray)
             {:obj sphere1 :pt [-1 0 0] :dist 1 :normal [-1 0 0] :ray ray})))
    ;; ...from both sides
    (let [ray {:start [2 0 0] :dir [-10000 0 0] :bounces 0}]
      (is (= (intersect sphere1 ray)
             {:obj sphere1 :pt [1 0 0] :dist 1 :normal [1 0 0] :ray ray})))
    ;; glancing hit
    (let [glancing {:start [3/5 10 0] :dir [0 -1 0] :bounces 0}
          interx (intersect sphere1 glancing)]
      (is (= (map cut (:pt interx)) [3/5 4/5 0]))
      (is (= (map cut (:normal interx)) [3/5 4/5 0]))
      (is (= (cut (:dist interx)) (- 10 4/5)))))
  (let [sphere2345 {:type :sphere :center [3 4 5] :radius 2}
        ray {:start [3 4 10] :dir [0 0 -5] :bounces 0}]
    ;; displaced origin
    (is (= (intersect sphere2345 ray)
           {:obj sphere2345 :pt [3 4 7] :dist 3 :normal [0 0 1] :ray ray}))))

(deftest intersect-plane
  (let [plane5 {:type :plane :pt [30 40 5] :normal [0 0 5]}]
    ;; from below...
    (let [ray {:start [0 0 0] :dir [0 0 1]}]
      (is (= (intersect plane5 ray)
             {:obj plane5 :pt [0 0 5] :dist 5 :normal [0 0 -1] :ray ray})))
    ;; ...and above (notice flipped normal)
    (let [ray {:start [0 0 10] :dir [0 0 -1]}]
      (is (= (intersect plane5 ray)
             {:obj plane5 :pt [0 0 5] :dist 5 :normal [0 0 1] :ray ray})))
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

;; Unit spheres laid out on the Y axis.
(def s0 {:type :sphere, :center [0 0 0], :radius 1}) ; [-1 1]
(def s3 {:type :sphere, :center [0 3 0], :radius 1}) ; [2 4]
(def s6 {:type :sphere, :center [0 6 0], :radius 1}) ; [5 7]
(def y-beads [s0 s3 s6])

(deftest ray-vs-scene
  ;; TODO order doesn't matter
  (let [ray {:start [0 -10 0] :dir [0 1 0] :bounces 0}]
    (is (= (ray-hits y-beads ray)
           [{:obj s0 :pt [0 -1 0] :dist 9 :normal [0 -1 0] :ray ray}
            {:obj s3 :pt [0 2 0] :dist 12 :normal [0 -1 0] :ray ray}
            {:obj s6 :pt [0 5 0] :dist 15 :normal [0 -1 0] :ray ray}])))
    ;; filter nils
  (let [ray {:start [0 1.5 0] :dir [0 1 0] :bounces 0}]
    (is (= (ray-hits y-beads ray)
           [{:obj s3 :pt [0 2 0] :dist 0.5 :normal [0 -1 0] :ray ray}
            {:obj s6 :pt [0 5 0] :dist 3.5 :normal [0 -1 0] :ray ray}])))
   ;; from one direction...
  (let [ray {:start [0 -10 0] :dir [0 1 0] :bounces 0}]
    (is (= (closest-hit (ray-hits y-beads ray))
           {:obj s0 :pt [0 -1 0] :dist 9 :normal [0 -1 0] :ray ray})))
   ;; ...and the other.
  (let [ray {:start [0 10 0] :dir [0 -20 0] :bounces 0}]
    (is (= (closest-hit (ray-hits y-beads ray))
           {:obj s6 :pt [0 7 0] :dist 3 :normal [0 1 0] :ray ray}))))

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

(deftest ambient-lighting
  (is (= (ambient {:settings {:ambient 0.1}}
                  {:obj {:material {:ambient {:color [20 30 40]}}}})
         [2 3 4])))

(deftest light-dir
  (is (= (to-light {:type :point :source [103 504 0]}
                   [100 500 0])
         [3/5 4/5 0]))
  (is (= (to-light {:type :directional :direction [3 0 4]}
                   [100 200 300])
         [(- 3/5) 0 (- 4/5)])))

(deftest light-distance-away
  (is (= (light-distance {:type :point, :source [100 204 303]} [100 200 300])
         5))
  (is (= (light-distance {:type :directional, :direction [1 2 3]} [4 5 6])
         Double/POSITIVE_INFINITY)))

(deftest conditional-dropper
  (is (= (drop-first-if even? [1 2 3]) [1 2 3]))
  (is (= (drop-first-if even? [2 3 4]) [3 4]))
  (is (= (drop-first-if even? nil) nil)))

(deftest approx-intersection-equiv
  (binding [*intersection-equiv-dist* 1]
    (let [basic {:obj "foo", :pt [0 1 2], :ray nil, :dist nil}]
      (is (interx= basic basic))
      (is (interx= basic (assoc basic :pt [0.1 1.1 1.9])))
      (is (not (interx= basic (assoc basic :pt [0 1 3.1]))))
      (is (not (interx= basic (assoc basic :obj "bar")))))))

(deftest shadows
  ;; empty scene
  (is (light-visible? {:pt [0 2 0], :obj s3}
                      {:type :point, :source [0 0 0]}
                      []))
  ;; nothing involved
  (is (light-visible? {:pt [20 -100 0], :obj {}}
                      {:type :point, :source [20 100 0]}
                      y-beads)) ; parallel
  (is (light-visible? {:pt [0 1.2 0], :obj {}}
                      {:type :point, :source [0 1.8 0]}
                      y-beads))   ; short
  ;; point light is in open space
  (let [light15 {:type :point, :source [0 1.5 0]}]
    ;; from center of s0 (surface interference)
    (is (not (light-visible? {:pt [0 0 0], :obj s0}
                             light15
                             y-beads)))
    ;; just off the surface
    (is (light-visible? {:pt [0 0.999999999 0], :obj s0}
                        light15
                        y-beads))
    ;; wrong exclude
    (is (not (light-visible? {:pt [0 0.999999999 0], :obj s3}
                             light15
                             y-beads))))
  ;; point light hidden in sphere
  (let [light3 {:type :point, :source [0 3 0]}]
    (is (not (light-visible? {:pt [0 1.5 0], :obj s0} light3 y-beads)))
    (is (not (light-visible? {:pt [0 1.5 0], :obj s3} light3 y-beads)))
    (is (light-visible? {:pt [0 2.5 0], :obj s3} light3 y-beads)))
  ;; directional
  (let [overhead {:type :directional, :direction [0 0 -1]}]
    (is (light-visible? {:pt [0 1.5 0], :obj s0} overhead y-beads))
    (is (not (light-visible? {:pt [0 0 0], :obj s0} overhead y-beads)))
    (is (light-visible? {:pt [0 0 1], :obj s0} overhead y-beads))
    (is (not (light-visible? {:pt [0 0 -1], :obj s0} overhead y-beads)))))

(deftest diffuse-lighting
  (let [overhead2 {:type :directional, :direction [0 0 -1], :I 2}
        dir45xz {:type :directional, :direction [-1 0 -1], :I 3}
        redmat {:diffuse {:color [0.3 0 0]}}
        planez0 {:type :plane, :normal [0 0 1], :pt [0 0 0], :material redmat}
        centerx {:obj planez0
                 :pt (:pt planez0)
                 :normal (:normal planez0)
                 :dist 10
                 :ray nil}]
    ;; full reflection
    (is (= (diffuse [planez0] centerx overhead2)
           [(* 0.3 2 1) 0 0]))
    ;; 45 degree incidence
    (is (= (map cut (diffuse [planez0] centerx dir45xz))
           (map cut [(* 0.3 3 (/ (Math/sqrt 2) 2)) 0 0])))))

(deftest specular-lighting
  (let [dir45xz {:type :directional, :direction [-1 0 -1], :I 3}
        redmat {:specular {:color [0.3 0 0], :exp 5}}
        planez0 {:type :plane, :normal [0 0 1], :pt [0 0 0], :material redmat}
        centerx {:obj planez0
                 :pt (:pt planez0)
                 :normal (:normal planez0)
                 :dist 10
                 :ray "?"}
        ray90 {:start [-1 0 1] :dir [1 0 -1]}
        ray45 {:start [0 0 1] :dir [0 0 -1]}
        cos225 (Math/cos (/ Math/PI 8))]
    ;; full reflection
    (is (= (map cut (specular [planez0] (assoc centerx :ray ray90) dir45xz))
           (map cut [(* 0.3 3 1) 0 0])))
    (is (= (map cut (specular [planez0] (assoc centerx :ray ray45) dir45xz))
           (map cut [(* 0.3 3 (Math/pow cos225 5)) 0 0])))))

(deftest reflection
  (is (= (reflect [0 1 0] [3 -4 0])
         [3 4 0]))
  (is (= (map cut (reflect (v/unit [0 -1 1]) [-1 1 0]))
         (map cut [-1 0 1]))))

(deftest colors
  (is (= (rgb->int [1 0 0.2]) 0xFF0033))
  (is (= (rgb->int [1.1 0.2 -5]) 0xFF3300)))

