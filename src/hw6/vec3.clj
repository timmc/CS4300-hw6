(ns hw6.vec3)

;;; All vectors here are 3-vectors of [x y z]

(defn mag
  "Magnitude of a vector."
  [[x y z]]
  (float (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn scale
  "Scale a vector by a constant."
  [[x y z] s]
  [(* x s) (* y s) (* z s)]) ; or (map (partial * s) v)

(defn unit
  "Compute the unit vector in the same direction."
  [v]
  (scale v (/ (mag v))))

(defn sum
  "Compute sum of two vectors."
  [[ax ay az] [bx by bz]]
  [(+ ax bx) (+ ay by) (+ az bz)])

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
