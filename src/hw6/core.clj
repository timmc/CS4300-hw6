(ns hw6.core
  (:require [hw6.parser :as p])
  (:import (java.io BufferedReader InputStreamReader))
  (:gen-class))

(defn -main
  "Run program from standard input and command-line args."
  [& args]
  (let [lines (line-seq (BufferedReader. (InputStreamReader. System/in)))]
    (println (p/parse lines))))

