(ns clango.stores
  (:require [clojure.string :as str])
  (:import [java.io File IOException]))

(defn- path-join [& paths]
  (str/join File/separator paths))

(defn file-store
  ([]
     (file-store (System/getProperty "user.dir")))
  ([root]
     (fn [name]
       (try
         (slurp (path-join root name))
         (catch IOException e "")))))

(defn map-store [template-map]
  (fn [name]
    (get template-map name "")))
