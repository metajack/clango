(ns clango.stores
  (:require [clojure.string :as str]
            [clango.parser :as parser])
  (:import [java.io File IOException]))

(defn- path-join [& paths]
  (str/join File/separator paths))

(defn file-store
  ([]
     (file-store (System/getProperty "user.dir")))
  ([root]
     (memoize
      (fn [name]
        (try
          (parser/parse (slurp (path-join root name)))
          (catch IOException e ""))))))

(defn map-store [template-map]
  (memoize
   (fn [name]
     (parser/parse (get template-map name "")))))
