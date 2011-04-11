(ns hw6.tracer
  (:require [hw6.vec3 :as v])
  (:import (java.awt Graphics2D)
           (java.awt.image BufferedImage)))

;;; A ray is a {:start [x y z] :dir [x y z] :bounces 0}
;;; An intersection is a {:obj <obj>, :pt [x y z], :dist f, :normal <unitvec>}

(defn along-ray
  "Compute {:pt, :dist} on a ray based on the number of multiples of :dir that
are required to reach it."
  [ray t]
  (let [v (v/scale (:dir ray) t)]
    {:pt (v/sum v (:start ray)), :dist (v/mag v)}))

;;;; Intersections

(defmulti intersect
  "Intersect an object and a ray. Return the first intersection or nil."
  (fn [o r] (:type o)))
(defmethod intersect :default [_ _] nil)
(defmethod intersect :sphere [{r :radius c :center :as obj}
                              {q :start d :dir :as ray}]
  (let [q-c (v/sum q (v/scale c -1))
        A (v/dot d d)
        B (v/dot (v/scale d 2) q-c)
        C (- (v/dot q-c q-c) (* r r))
        det (- (* B B) (* 4 A C))]
    (when-not (neg? det)
      (let [t1 (/ (+ (- B) (Math/sqrt det)) (* 2 A))
            t2 (/ (- (- B) (Math/sqrt det)) (* 2 A))
            [t1 t2] (if (< t1 t2) [t1 t2] [t2 t1])
            t (when (> t2 0) (if (> t1 0) t1 t2))]
        (when t
          (let [{pt :pt, dist :dist} (along-ray ray t)
                normal (v/unit (v/<-pts c pt))]
            {:obj obj, :pt pt, :dist dist, :normal normal}))))))
(defmethod intersect :plane [{pt :pt normal :normal :as obj}
                             {q :start d :dir :as ray}]
  (let [unormal (v/unit normal)
        dist-origin (v/dot unormal pt)
        angle-from-perp (v/dot d unormal)]
    (if (zero? angle-from-perp)
      nil
      (let [t (/ (- dist-origin (v/dot q unormal)) angle-from-perp)]
        (when (pos? t)
          (let [{pt :pt, dist :dist} (along-ray ray t)
                ;; Pick the normal that bounces back against the ray
                norm-sigmult (- (Math/signum (double angle-from-perp)))
                unormal (v/scale unormal norm-sigmult)]
            {:obj obj, :pt pt, :dist dist, :normal unormal}))))))

(defn ray-hits
  "Compute a seq of all object intersections in the scene with the given ray."
  [scene ray]
  (filter (complement nil?) (map #(intersect % ray) (:objects scene))))

(defn closest-hit
  "Find the closest ray hit in the scene, or nil."
  [hits]
  (if (seq hits)
    (reduce (fn closer [h1 h2] (if (< (:dist h1) (:dist h2)) h1 h2)) hits)
    nil))

(def ^{:doc "Camera's field of view in degrees." :dynamic true}
  *camera-fov* 60)

(defn pixel->cam-coord
  "Convert a canvas pixel to an image-plane point in the camera's coordinates."
  [w h x y]
  (let [flipspect (float (- (/ h w)))
        ;; Use the center of each pixel
        pix-mid (float 0.5)
        ;; half plane and half FOV
        half-fov (* Math/PI (/ (/ *camera-fov* 2) 180)) ; in radians now
        implane-z (- (/ 1/2 (Math/tan half-fov)))]
    [(- (/ (+ x pix-mid) w) (float 0.5))
     (* flipspect (- (/ (+ y pix-mid) h) (float 0.5)))
     implane-z]))

(defn image-rays
  "Returns a seq of cooresponding canvas pixels, points in the image plane, and
rays from the viewpoint as {:pixel [x y], :ray <ray>}."
  [camera w h]
  #_
  (let [eye (get-in camera [:pose :start])]
    (for [x (range w)
          y (range h)]
      ;; TODO instead, iterate over world points after computing corners
      (let [image-plane-pt (pixel->cam-coord w h x y)
            world-pt (v/xform (:xfrom camera) image-plane-pt)
            pixel-ray {:start eye :dir (v/<-pts eye world-pt) :bounces 0}]
        {:pixel [x y]
         ;; :pt image-plane-pt
         :ray pixel-ray})))
  ;; XXX For now, camera is at [0 0 10] with image plane at z=9 and 90 deg FOV
  (let [altitude 50
        eye [0 0 altitude]]
    (for [x (range w)
          y (range h)]
      (let [via [(- (/ (+ x (float 0.5)) w) (float 0.5))
                 (- (- (/ (+ y (float 0.5)) h) (float 0.5)))
                 (- altitude 1)]]
        {:pixel [x y] :ray {:start eye :dir (v/<-pts eye via) :bounces 0}}))))

;;;; Lighting

(defn ambient
  "Calculate the [r g b] ambient lighting component of a ray intersection."
  [scene ray interx]
  (let [amb-color (-> interx :obj :material :ambient :color)
        amb-comp (v/scale amb-color (-> scene :settings :ambient))]
    amb-comp))

(defmulti light-to
  "Determine the unit direction vector of an inbound light source given
an [x y z] pt and a unit normal vector."
  (fn [light pt] (:type light)))
(defmethod light-to :directional
  [light pt]
  (v/unit (:direction light)))
(defmethod light-to :point
  [light pt]
  (v/unit (v/<-pts (:source light) pt)))

(defn diffuse-1
  "Given a single light and an intersection, produce the diffuse color
contribution."
  [scene ray interx diffuse-mat light]
  (let [to-light (v/neg (light-to light (:pt interx)))
        cos (v/dot (:normal interx) to-light)]
    ;; TODO shadows
    (if (neg? cos)
      [0 0 0]
      (let [I (:I light)
            dc (:color diffuse-mat)]
        ;; Using a white light source, a.k.a. [I I I]
        ;; Otherwise we would do (v/scale (v/elop * light-color mat-color) cos)
        (v/scale dc (* I cos))))))

(defn diffuse
  "Calculate the [r g b] diffuse lighting component of a ray intersection. This
implements Lambertian shading."
  [scene ray interx]
  (if (-> scene :settings :diffuse?)
    (let [diff-mat (-> interx :obj :material :diffuse)]
      (apply v/sum (map (partial diffuse-1 scene ray interx diff-mat)
                        (:lights scene))))
    [0 0 0]))

(defn specular-1 ;; FIXME way too dim for point source
  "Given a single light and an intersection, produce the specular color
contribution."
  [scene ray interx specular-mat light]
  (let [to-light (v/neg (light-to light (:pt interx)))
        to-viewer (v/unit (v/neg (:dir ray)))
        halfway (v/avg to-light to-viewer)
        cos (v/dot (:normal interx) halfway)]
    ;; TODO shadows
    (if (neg? cos)
      [0 0 0]
      (let [cosp (Math/pow cos (:exp specular-mat))
            I (:I light)
            sc (:color specular-mat)]
        ;; Using a white light source, a.k.a. [I I I]
        ;; Otherwise we would do (v/scale (v/elop * light-color mat-color) cosp)
        (v/scale sc (* I cosp))))))

(defn specular
  "Calulate the [r g b] specular lighting component of a ray intersection. This
implements Blinn-Phong shading."
  [scene ray interx]
  (if (-> scene :settings :specular?)
    (let [spec-mat (-> interx :obj :material :specular)]
      (apply v/sum (map (partial specular-1 scene ray interx spec-mat)
                        (:lights scene))))
    [0 0 0]))

(defn ray->rgb
  "Given a scene and a ray, produce an [r g b] intensity value."
  [scene ray]
  (let [hits (ray-hits scene ray)
        interx (closest-hit hits)]
    (if interx
      (v/sum (ambient scene ray interx)
             (diffuse scene ray interx)
             (specular scene ray interx))
      [0 0 0])))

(defn rgb->int
  "Given an [r g b] intensity, clamp components to unit range and produce an
RGB int."
  [[r g b]]
  (let [convert-comp (fn [c] (max 0 (min 255 (int (* 255 c)))))]
    (+ (bit-shift-left (convert-comp r) 16)
       (bit-shift-left (convert-comp g) 8)
       (convert-comp b))))

;;;; Main loop

(defn render
  [^Graphics2D g, scene, ^Integer w, ^Integer h]
  (let [bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (doseq [{[vx vy] :pixel
             pixel-ray :ray} (image-rays (:camera scene) w h)]
      (let [rgb (ray->rgb scene pixel-ray)]
        (.setRGB bi vx vy (rgb->int rgb)))
      (.drawImage g bi nil 0 0))))

