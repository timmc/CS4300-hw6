(ns hw6.test.parser
  (:use [hw6.parser])
  (:use [clojure.test])
  (:require [hw6.vec3 :as v]))

(deftest vertices
  (is (= (parse-line {:vertices [{:start [0 0 0] :dir [10 20 30]}]}
                     "vv" "1" "2" "3" "4" "5" "6")
         {:vertices [{:start [0 0 0] :dir [10 20 30]}
                     {:start [1 2 3] :dir [4 5 6]}]})))

(deftest spheres
  (is (= (parse-line {:objects []} "ss" "2")
         {:objects [{:type :sphere :i 2 :material nil}]})))

(deftest single-sphere
  (let [parsed (parse ["vv -10 75 60 1 -7.5 -6"
                       "cc 0"
                       "vv 100 100 0 0 0 0"
                       "pl 1 0.5"
                       "vv 0 10 0 10 0 0"
                       "ss 2"
                       "vv 0.0 0.0 0.0 0 1 0"
                       "ps 3"])
        verts (:vertices parsed)
        sphere (first (:objects parsed))
        cam (:camera parsed)]
    (is (= verts [{:start [-10 75 60] :dir [1 -7.5 -6]}
                  {:start [100 100 0] :dir [0 0 0]}
                  {:start [0 10 0] :dir [10 0 0]}
                  {:start [0 0 0] :dir [0 1 0]}]))
    (is (= (:type sphere) :sphere))
    (is (= (:center sphere) [0 10 0]))
    (is (= (:radius sphere) 10))
    (is (= (:pose cam) (get verts 0)))
    #_
    (let [transform (:xfrom cam) ;; TODO
          cent (v/xform transform [0 0 1])]
      (is (= cent [-1 7.5 6])))))
