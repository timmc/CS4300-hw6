(ns hw6.core
  (:require [clojure.contrib.string :as str])
  (:require [hw6.parser :as p]
            [hw6.tracer :as rt])
  (:import (java.io BufferedReader StringReader InputStreamReader)
           (java.awt Dimension Graphics2D)
           (javax.swing JComponent JFrame UIManager SwingUtilities))
  (:gen-class))


(defn make-canvas
  "Make a canvas that renders once."
  [scene settings]
  (let [jc (proxy [JComponent] []
	     (paint [^Graphics2D g]
               (rt/render g scene (.getWidth this) (.getHeight this)))
             (update [_]))]
    (doto jc
      (.setDoubleBuffered true)
      (.setPreferredSize (Dimension. (:w settings) (:h settings))))))

(defn launch
  "Open a window and render the scene."
  [scene settings]
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (let [fr (JFrame. "CS4300 HW6 - TimMc")]
    (doto fr
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (make-canvas scene settings))
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

(defn read-posint
  "Read a positive integer value from the command line."
  [remain flag field]
  (if-let [val (first remain)]
    (if-let [i (maybe-parse-int val)]
      (if (< 0 i)
        i
        (fail (str "Negative " field " not allowed: " i)))
      (fail (str "Could not parse " field " as integer: " val)))
    (fail (str "Expected " field " after " flag))))

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
                   ;; :diffuse?
                   ;; :specular?
                   ;; :shadows?
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
                                  (read-posint remain "width" flag))
                        (rest remain))
            "-h" (recur (assoc-in settings [:h]
                                  (read-posint remain "height" flag))
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
            (fail (str "Unknown argument: " flag)))))))

(defn -main
  "Run program from standard input and command-line args."
  [& args]
  (let [settings (read-arguments args)
        reader (if (seq (:in settings))
                 (StringReader. (str/join \newline (map slurp (:in settings))))
                 (InputStreamReader. System/in))]
    (let [lines (line-seq (BufferedReader. reader))
          scene (p/parse lines)
          overrides (select-keys settings [:diffuse? :specular? :shadows?])
          scene (update-in scene [:settings] merge overrides)]
      (println (format "Loaded %d objects and %d light sources."
                       (count (:objects scene))
                       (count (:lights scene))))
      (SwingUtilities/invokeLater (partial launch scene settings)))))

