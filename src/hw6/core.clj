(ns hw6.core
  (:require [clojure.contrib.string :as str])
  (:require [hw6.parser :as p]
            [hw6.tracer :as rt])
  (:import (java.io BufferedReader StringReader InputStreamReader)
           (java.awt Dimension Graphics2D)
           (java.awt.image BufferedImage)
           (javax.swing JComponent JFrame UIManager SwingUtilities))
  (:gen-class))


(def ^{:doc "Scene render progress map. :status may be :init, :working, or
:done, and :completion is a float from [0 1]."}
  *render-status* (ref {:status :init, :completion 0}))

(defn start-render
  "Start a renderer writing to the given BufferedImage."
  [scene, bi, ^JComponent canvas]
  ;; TODO use a normal Thread, since exceptions only appear on deref
  (future (try (rt/render scene bi *render-status*)
               (catch Exception e (.printStackTrace e)))
          ;; one final repaint to catch the last bit of data.
          (.repaint canvas)))

(defn make-canvas
  "Make a canvas that renders once."
  [scene bi settings]
  (let [jc (proxy [JComponent] []
	     (paint [^Graphics2D g]
               (.drawImage g bi nil 0 0)
               (when-not (= (:status @*render-status*) :done)
                 (future (Thread/sleep 1000)
                         (.repaint this))))
             (update [_]))]
    (doto jc
      (.setDoubleBuffered true)
      (.setPreferredSize (Dimension. (:w settings) (:h settings))))))

(defn launch
  "Open a window and render the scene."
  [scene settings]
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [{w :w h :h} settings
        fr (JFrame. "CS4300 HW6 - TimMc")
        bi (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        canvas (make-canvas scene bi settings)]
    (start-render scene bi canvas)
    (doto fr
      ;; TODO Just dispose, but also kill the render thread.
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.add canvas)
      (.pack)
      (.setResizable false)
      (.setVisible true))))

(defn fail
  "Fail with a message."
  [msg]
  (binding [*out* *err*]
    (println "Failed:" msg))
  (System/exit 1))

(defn maybe-parse-int
  "Try parsing a base-10 int. If fail, return nil."
  [s]
  (try (Integer/parseInt s 10)
       (catch NumberFormatException nfe
         nil)))

(defn read-int
  "Read an integer value from the command line, with optional validator."
  ([remain flag field]
     (read-int remain flag field nil))
  ([remain flag field validator]
     (if-let [val (first remain)]
       (if-let [i (maybe-parse-int val)]
         (if (or (nil? validator) (validator i))
           i
           (fail (str "Bad value for " field ": " i)))
         (fail (str "Could not parse " field " as integer: " val)))
       (fail (str "Expected " field " after " flag)))))

(defn read-boolean
  "Read a boolean (true yes on 1, false no off 0) value from the command line."
  [remain flag field]
  (if-let [val (first remain)]
    (if (#{"true" "yes" "on" "1"} val)
      true
      (if (#{"false" "no" "off" "0"} val)
        false
        (fail (str "Could not parse " field " as a boolean: " val))))
    (fail (str "Expected " field " after " flag))))

(defn read-arguments
  "Read command-line arguments into a settings map:
-f * :in = vector of filenames"
  [args]
  (loop [settings {:in []
                   :w 512
                   :h 512
                   ;; plus the scene-settings-keys
                   }
         args args]
    (if (empty? args)
      settings
      (let [flag (first args)
            remain (rest args)]
        (condp = flag
            "-f" (if-let [val (first remain)]
                   (recur (update-in settings [:in] conj val)
                          (rest remain))
                   (fail "Expected filename after -f"))
            "-w" (recur (assoc-in settings [:w]
                                  (read-int remain "width" flag #(> % 0)))
                        (rest remain))
            "-h" (recur (assoc-in settings [:h]
                                  (read-int remain "height" flag #(> % 0)))
                        (rest remain))
            "-ld" (recur (assoc-in settings [:diffuse?]
                                   (read-boolean
                                    remain "diffuse light" "-ld"))
                         (rest remain))
            "-ls" (recur (assoc-in settings [:specular?]
                                   (read-boolean
                                    remain "specular light" "-ls"))
                         (rest remain))
            "-sh" (recur (assoc-in settings [:shadows?]
                                   (read-boolean
                                    remain "shadows" "-sh"))
                         (rest remain))
            "-rf" (recur (assoc-in settings [:reflectivity]
                                   (/ (read-int remain "reflectivity %" flag)
                                      100.0))
                         (rest remain))
            "-ml" (recur (assoc-in settings [:mirror-limit]
                                   (read-int remain "mirror limit" flag
                                             #(>= % 0)))
                         (rest remain))
            (fail (str "Unknown argument: " flag)))))))

(defn -main
  "Run program from standard input and command-line args."
  [& args]
  (let [settings (read-arguments args)
        reader (if (seq (:in settings))
                 (StringReader. (str/join \newline (map slurp (:in settings))))
                 (InputStreamReader. System/in))]
    (let [lines (line-seq (BufferedReader. reader))
          scene (try (p/parse lines)
                     (catch Exception e
                       (fail (.getMessage e))))
          overrides (select-keys settings
                                 (set (keys (:settings p/empty-scene))))
          scene (update-in scene [:settings] merge overrides)]
      (println (format "Loaded %d objects and %d light sources."
                       (count (:objects scene))
                       (count (:lights scene))))
      (SwingUtilities/invokeLater (partial launch scene settings)))))

