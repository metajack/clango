(ns clango.parsemain
  (:require [clango.core :as c]
            [clango.parser :as p])
  (:gen-class)
  (:import [clango.antlr TemplateParser]))

(defn -main [a & args]
  (try
    (println (p/parse a))
    (catch Exception e (println (str e) (.printStackTrace e)))))
