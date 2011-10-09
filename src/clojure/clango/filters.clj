(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(defn- coerce-int [s]
  (if (string? s)
    (Integer/valueOf s)
    (int s)))

(defn add [x y]
  (+ (coerce-int x) (coerce-int y)))

(defn addslashes [x]
  (-> x
      (str/replace "\\" "\\\\")
      (str/replace "'" "\\'")
      (str/replace "\"" "\\\"")))

(defn capfirst [x]
  (str/capitalize x))

(defn center [s width]
  (let [w (coerce-int width)
        lead (int (/ (- w (count s)) 2))
        padding (apply str (take lead (cycle " ")))]
    (if (odd? w)
      (apply str padding s padding " ")
      (apply str padding s padding))))

(defn cut [s src]
  (str/replace s src ""))

(defn default [s df]
  (if (str/blank? s)
    df
    s))

(defn default-if-none [s df]
  (if s
    s
    df))

(defn divisibleby [num div]
  (zero? (mod (coerce-int num) (coerce-int div))))

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
