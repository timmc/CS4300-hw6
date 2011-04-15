(ns hw6.tracer
  (:require [hw6.vec3 :as v])
  (:import (java.awt Graphics2D)
           (java.awt.image BufferedImage)))

;;; A ray is a {:start [x y z] :dir [x y z] :bounces 0}
;;; An intersection is a {:obj <obj>, :pt [x y z], :dist f, :normal <unitvec>,
;;;    :ray <ray>}

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
            {:obj obj, :pt pt, :dist dist, :normal normal, :ray ray}))))))
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
            {:obj obj, :pt pt, :dist dist, :normal unormal, :ray ray}))))))

(defn ray-hits
  "Compute a seq of all object intersections in the scene with the given ray."
  [objects ray]
  (filter (complement nil?) (map #(intersect % ray) objects)))

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
        half-plane (float 0.5)
        ;; half plane and half FOV
        half-fov (* Math/PI (/ (/ *camera-fov* 2) 180)) ; in radians now
        implane-z (- (/ half-plane (Math/tan half-fov)))]
    [(- (/ (+ x pix-mid) w) half-plane)
     (* flipspect (- (/ (+ y pix-mid) h) half-plane))
     implane-z]))

(defn image-rays
  "Returns a seq of cooresponding canvas pixels, points in the image plane, and
rays from the viewpoint as {:pixel [x y], :ray <ray>}."
  [camera w h]
  (let [eye (get-in camera [:pose :start])]
    (for [y (range h)
          x (range w)]
      ;; TODO instead, iterate over world points after computing corners
      (let [image-plane-pt (pixel->cam-coord w h x y)
            world-pt (v/xform (:xfrom camera) image-plane-pt)
            pixel-ray {:start eye :dir world-pt :bounces 0}]
        {:pixel [x y]
         ;; :pt image-plane-pt
         :ray pixel-ray}))))

;;;; Lighting

(defn ambient
  "Calculate the [r g b] ambient lighting component of a ray intersection."
  [scene interx]
  (let [amb-color (-> interx :obj :material :ambient :color)
        amb-comp (v/scale amb-color (-> scene :settings :ambient))]
    amb-comp))

(defmulti to-light
  "Determine the unit direction vector from an [x y z] point towards a light."
  (fn [light pt] (:type light)))
(defmethod to-light :directional
  [light pt]
  (v/unit (v/neg (:direction light))))
(defmethod to-light :point
  [light pt]
  (v/unit (v/<-pts pt (:source light))))

(defmulti light-distance
  "Determine the distance to the light source. (May be infinite.)"
  (fn [light pt] (:type light)))
(defmethod light-distance :directional
  [light pt]
  Double/POSITIVE_INFINITY)
(defmethod light-distance :point
  [light pt]
  (v/mag (v/<-pts (:source light) pt)))

(defn drop-first-if
  "If the seq is not empty, drop the first element if it matches the predicate."
  [pred s]
  (when-let [s (seq s)]
    (if (pred (first s))
      (rest s)
      s)))

(def ^{:doc "Radius of approximate equivalence for intersections" :dynamic true}
  *intersection-equiv-dist* 0.00001)

(defn interx=
  "Detect if two intersections are probably the same."
  [i1 i2]
  (and (= (:obj i1) (:obj i2))
       (< (v/mag (v/<-pts (:pt i1) (:pt i2))) *intersection-equiv-dist*)))

(defn light-visible?
  [interx light objects]
  (let [origin (:pt interx)
        ray {:start origin, :dir (to-light light origin), :bounces 0}
        close-hits (sort-by :dist (ray-hits objects ray))
        candidates (drop-first-if (partial interx= interx) close-hits)]
    (or (empty? candidates)
        (> (:dist (first candidates)) (light-distance light origin)))))

(defn diffuse
  "Calculate the [r g b] diffuse lighting component for one light and one ray
intersection (or nil.) This implements Lambertian shading."
  [objects interx light]
  (let [to-light (to-light light (:pt interx))]
    (when (light-visible? interx light objects)
      (let [cos (v/dot (:normal interx) to-light)]
        (when-not (neg? cos)
          (let [I (:I light)
                dc (-> interx :obj :material :diffuse :color)]
            (v/scale dc (* I cos))))))))

(defn specular ;; FIXME way too dim for point source
  "Given a single light and an intersection, produce the specular color
contribution."
  [interx light]
  (let [to-light (to-light light (:pt interx))
        to-viewer (v/unit (v/neg (:dir (:ray interx))))
        halfway (v/avg to-light to-viewer)
        cos (v/dot (:normal interx) halfway)]
    ;; TODO shadows
    (when-not (neg? cos)
      (let [mat (-> interx :obj :material :specular)
            cosp (Math/pow cos (:exp mat))
            I (:I light)
            sc (:color mat)]
        ;; Using a white light source, a.k.a. [I I I]
        ;; Otherwise we would do (v/scale (v/elop * light-color mat-color) cosp)
        (v/scale sc (* I cosp))))))

(defn ray->rgb
  "Given a scene and a ray, produce an [r g b] intensity value, or nil."
  [scene ray]
  (let [objects (:objects scene)
        hits (ray-hits objects ray)
        interx (closest-hit hits)]
    (when interx
      (let [lights (:lights scene)
            amb (ambient scene interx)
            diffs (when (-> scene :settings :diffuse?)
                    (map (partial diffuse objects interx) lights))
            specs (when (-> scene :settings :specular?)
                    (map (partial specular interx) lights))
            rgbs (filter (complement nil?) (concat [amb] diffs specs))]
        (when (seq rgbs)
          (apply v/sum rgbs))))))

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
  [scene, ^BufferedImage bi, render-status]
  (let [w (.getWidth bi)
        h (.getHeight bi)]
    (dosync (ref-set render-status
                     (assoc @render-status :status :working)))
    (doseq [{[vx vy] :pixel
             pixel-ray :ray} (image-rays (:camera scene) w h)]
      (let [rgb (ray->rgb scene pixel-ray)]
        (when rgb
          (.setRGB bi vx vy (rgb->int rgb)))))
    (dosync (ref-set render-status
                     (assoc @render-status :status :done)))))

