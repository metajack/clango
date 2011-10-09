(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(defn add [x y]
  (let [to-integer #(if (string? %) (Integer/valueOf %) (int %))]
    (+ (to-integer x) (to-integer y))))

(defn addslashes [x]
  (-> x
      (str/replace "\\" "\\\\")
      (str/replace "'" "\\'")
      (str/replace "\"" "\\\"")))

(defn capfirst [x]
  (str/capitalize x))

(defn center [s width]
  (let [lead (int (/ (- width (count s)) 2))
        padding (apply str (take lead (cycle " ")))]
    (if (odd? width)
      (apply str padding s padding " ")
      (apply str padding s padding))))

(defn cut [s src]
  (str/replace s src ""))

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
