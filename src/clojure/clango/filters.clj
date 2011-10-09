(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(defn- coerce-int [s]
  (if (string? s)
    (Integer/valueOf s)
    (int s)))

(defn- coerce-double [s]
  (if (string? s)
    (Double/valueOf s)
    (double s)))

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

(defn filesizeformat [size]
  (let [abbrevs ["B" "kB" "MB" "GB" "TB"]
        sizes (take (count abbrevs) (iterate #(/ % 1000.0) (coerce-int size)))
        zip (map (fn [x y] [x y]) sizes abbrevs)
        [sz abr] (core/first (drop-while #(> (core/first %) 1000) zip))]
    (if (integer? sz)
      (format "%d %s" sz abr)
      (format "%.1f %s" sz abr))))

(defn first [x]
  (core/first x))

(defn fix-ampersands [s]
  (str/replace s #"&(?!\p{Alpha}+;)" "&amp;"))

(defn floatformat
  ([n]
     (floatformat n -1))
  ([n p]
     (let [num (coerce-double n)
           pint (coerce-int p)
           precision (if (and (neg? pint) (== (int num) num))
                       0
                       (Math/abs pint))]
       (format (str "%." precision "f") num))))

(defn join
  ([x]
     (str/join x))
  ([x s]
     (str/join s x)))

(defn last [x]
  (core/last x))

(defn length [x]
  (count x))
