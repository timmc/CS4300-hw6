(ns hw6.parser
  (:require [clojure.contrib.string :as str])
  (:require [hw6.vec3 :as v]))

;;; All data represented as maps:
;;; - vertex is {:start [x y z] :dir [x y z]}
;;; - material is :ambient={:color=[r g b]} :diffuse={:color=[r g b]}
;;;     :specular={:color=[r g b] :exp=int}
;;; - settings is :diffuse? :specular? :shadows? :mirror-limit :ambient
;;; - camera is :i => :pose=vertex :xfrom=3x3mat (from camera to world)
;;; - light is :type=#{:point :directional} :I :i
;;;   - :point also has :source (from :i=>vertex)
;;;   - :directional also has :direction (from :i=>vertex)
;;; - object is :type=#{:sphere :plane :triangle} :material=material + more
;;;   - :plane also has :i => :pt=[x y z] :normal=[x y z]
;;;   - :triangle also has :i :j :k => :v0 :v1 :v2 (all vert/normals)
;;;   - :sphere also has :i => :center=[x y z] :radius=float

;;; A scene is a map of:
;;; - :vertices = vector, where vertex ID = index
;;; - :camera = camera
;;; - :settings = settings
;;; - :lights = vector of lights
;;; - :objects = vector of objects
;;; - :last-material = material

;;;; Line by line

(def empty-scene
  {:vertices []
   :camera {:pose {:start [0 0 0] :dir [0 0 -1]}}
   :settings {:diffuse? true
              :specular? true
              :shadows? true
              :mirror-max 1
              :ambient-intensity 1.0}
   :lights []
   :objects []
   :last-material {:ambient {:color [0.2 0.2 0.2]}
                   :diffuse {:color [1.0 1.0 1.0]}
                   :specular {:color [1.0 1.0 1.0] :exp 64}}})

(defn parse-float [x] (Float/parseFloat x))
(defn parse-int [x] (Integer/parseInt x 10))

(defmulti parse-line
  "Fold the given space-split line into the scene. (Return updated scene.)"
  (fn [scene code & args] code))
(defmethod parse-line :default [scene unknown & _]
  (when-not (or (#{"ts" "tx" "rx" "hx" "ux" "ix" "so" "eo" "io" "rv"} unknown)
                (= (.substring unknown 0 2) "##"))
    (throw (Exception. (str "Could not parse line starting with " unknown))))
  scene)
;; vertices
(defmethod parse-line "vv" [scene _ & args]
  (let [[x y z dx dy dz] (map parse-float args)]
    (update-in scene [:vertices]
               conj {:start [x y z] :dir [dx dy dz]})))
;; materials
(defmethod parse-line "am" [scene _ & args]
  (let [[r g b] (map parse-float args)]
    (assoc-in scene [:last-material :ambient]
              {:color [r g b]})))
(defmethod parse-line "dm" [scene _ & args]
  (let [[r g b] (map parse-float args)]
    (assoc-in scene [:last-material :diffuse]
              {:color [r g b]})))
(defmethod parse-line "sm" [scene _ r g b p]
  (let [[r g b] (map parse-float [r g b])
        p (parse-int p)]
    (assoc-in scene [:last-material :specular]
              {:color [r g b] :exp p})))
;; objects
(defmethod parse-line "ss" [scene _ i]
  (update-in scene [:objects] conj
             {:type :sphere
              :i (parse-int i)
              :material (:last-material scene)}))
(defmethod parse-line "ps" [scene _ i]
  (update-in scene [:objects] conj
             {:type :plane
              :i (parse-int i)
              :material (:last-material scene)}))
;; settings
(defmethod parse-line "se" [scene _ d s a m I]
  (assoc-in scene [:settings]
            {:diffuse? (= d "d")
             :specular? (= s "s")
             :shadows? (= a "a")
             :mirror-limit (parse-int m)
             :ambient (parse-float I)}))
;; lights
(defmethod parse-line "pl" [scene _ i I]
  (update-in scene [:lights]
             conj {:type :point :i (parse-int i) :I (parse-float I)}))
(defmethod parse-line "dl" [scene _ i I]
  (update-in scene [:lights]
             conj {:type :directional :i (parse-int i) :I (parse-float I)}))
;; camera
(defmethod parse-line "cc" [scene _ i]
  (assoc-in scene [:camera] {:i (parse-int i)}))

;;;; Fill-in

(defn get-vertex
  [scene i]
  (get-in scene [:vertices i]))

(defmulti expand-object
  "Expand an object based on other scene data."
  (fn [obj scene] (:type obj)))
(defmethod expand-object :sphere [sphere scene]
  (if (and (:center sphere) (:radius sphere))
    sphere
    (let [vert (get-vertex scene (:i sphere))
          radius (v/mag (:dir vert))]
      (assoc sphere :center (:start vert) :radius radius))))
(defmethod expand-object :plane [plane scene]
  (if (and (:pt plane) (:normal plane))
    plane
    (let [vert (get-vertex scene (:i plane))]
      (assoc plane :pt (:start vert) :normal (:dir vert)))))
(defmethod expand-object :triangle [triangle scene]
  (if (and (:v0 triangle) (:v1 triangle) (:v2 triangle))
    triangle
    (assoc triangle
      :v0 (get-vertex scene (:i triangle))
      :v1 (get-vertex scene (:j triangle))
      :v2 (get-vertex scene (:k triangle)))))

(defn expand-light [light scene]
  (let [vertex (get-vertex scene (:i light))]
    (condp = (:type light)
        :point (if (:source light)
                 light
                 (assoc light :source (:start vertex)))
        :directional (if (:direction light)
                       light
                       (assoc light :direction (:dir vertex))))))
(defn expand-camera [camera scene]
  (let [camera (if (:pose camera)
                 camera
                 (assoc camera :pose (get-vertex scene (:i camera))))]
    (if (:xfrom camera)
      camera
      (let [{d :dir :as pose} (:pose camera)
            zcu (v/unit (v/scale d -1))
            xc (v/cross d [0 1 0])
            xcu (v/unit xc)
            ycu (v/cross zcu xcu)
            xfrom (v/xformer xcu ycu zcu)]
        (when (< (v/mag xc) 0.00001) ; TODO revert to older camera
          (throw (Exception. "Camera xc too close to zero.")))
        (assoc camera :xfrom xfrom)))))

(defn expand
  "Fill in vertex values by index, etc."
  [scene]
  (-> scene
      (update-in ,,, [:objects] (fn [o] (map #(expand-object % scene) o)))
      (update-in ,,, [:lights] (fn [l] (map #(expand-light % scene) l)))
      (update-in ,,, [:camera] expand-camera scene)))

;;;; Main loop

(defn parse
  "Parse a sequence of lines into a world"
  [lines]
  (let [lines (->> lines
                   (filter (complement str/blank?) ,,,)
                   (map (partial str/split #"\s+") ,,,)
                   (filter (complement nil?) ,,,))]
    (expand (reduce (partial apply parse-line) empty-scene lines))))

