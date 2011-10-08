(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(defn first [x]
  (core/first x))

(defn join
  ([x]
     (str/join x))
  ([x s]
     (str/join s x)))

(defn last [x]
  (core/last x))

(defn length [x]
  (count x))
