(ns org.timmc.cs4300-hw6.vec3
  (:use [incanter.core :only (matrix mmult mult trans)]))

;;; All vectors here are 3-vectors of [x y z]

(defn mag
  "Magnitude of a vector."
  [[x y z]]
  (float (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn scale
  "Scale a vector by a constant."
  [[x y z] s]
  [(* x s) (* y s) (* z s)]) ; or (map (partial * s) v)

(defn neg
  "Negate a vector's direction."
  [v]
  (scale v -1))

(defn unit
  "Compute the unit vector in the same direction. Given a zero-length vector,
returns it unchanged."
  [v]
  (let [m (mag v)]
    (if (zero? m)
      v
      (scale v (/ m)))))

(defn sum
  "Compute sum of two vectors."
  ([] [0 0 0])
  ([v] v)
  ([[ax ay az] [bx by bz]]
     [(+ ax bx) (+ ay by) (+ az bz)])
  ([a b & more]
     (reduce sum (sum a b) more)))

(defn diff
  "Convenience method for vector subtraction."
  [a b]
  (sum a (scale b -1)))

(defn <-pts
  "Compute a vector from start and end [x y z] points."
  [start end]
  (diff end start))

(defn dot
  "Compute the dot product of two vectors."
  [[ax ay az] [bx by bz]]
  (+ (* ax bx) (* ay by) (* az bz))) ; or (apply + (map * a b))

(defn cross
  "Compute cross product."
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn elop
  "Map an operator across the corresponding elements of multiple vectors."
  [op & vs]
  (vec (apply map op vs)))

(defn avg
  "Average multiple vectors together."
  [& vs]
  (scale (apply sum vs) (/ (count vs))))

(defn xformer
  "Create a 3x3 transformation matrix from 3 basis vectors."
  [xb yb zb]
  (trans (matrix [xb yb zb])))

(defn xform
  "Rotate, scale, and shear a vector in 3D using a 3x3 matrix."
  [mat v]
  (into [] (mmult mat v)))