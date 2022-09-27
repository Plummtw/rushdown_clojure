(ns rushhour.main
  ;; any :require and/or :import clauses
  (:refer-clojure)
  ;; (:require [clojure.string :as string])
  ;; (:import java.io.File)
  (:gen-class))

(defn -main [& _args]
  (println "Hello!"))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn one []
  "one")
