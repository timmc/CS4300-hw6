(ns hw6.core
  (:require [hw6.parser :as p])
  (:import (java.io BufferedReader StringReader InputStreamReader))
  (:require [clojure.contrib.string :as str])
  (:gen-class))


(defn fail
  "Fail with a message."
  [msg]
  (binding [*out* *err*]
    (println "Failed:" msg))
  (System/exit 1))

(defn read-arguments
  "Read command-line arguments into a settings map:
-f * :in = vector of filenames"
  [args]
  (loop [settings {:in []}
         args args]
    (if (empty? args)
      settings
      (let [flag (first args)
            remain (rest args)]
        (condp = flag
            "-f" (do
                   (when (empty? remain)
                     (fail "Expected filename after -f"))
                   (recur (update-in settings [:in]
                                     conj (first remain))
                          (rest remain)))
            (fail (str "Unknown argument: " flag)))))))

(defn -main
  "Run program from standard input and command-line args."
  [& args]
  (let [settings (read-arguments args)
        reader (if (seq (:in settings))
                 (StringReader. (str/join \newline (map slurp (:in settings))))
                 (InputStreamReader. System/in))]
    (let [lines (line-seq (BufferedReader. reader))
          scene (p/parse lines)]
      (println (format "Loaded %d objects and %d light sources."
                       (count (:objects scene))
                       (count (:lights scene))))
      (println "Scene parsed as:" scene) ;XXX
      )))

