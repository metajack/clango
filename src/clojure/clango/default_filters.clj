(ns clango.default-filters
  (:refer-clojure :exclude [first last reverse])
  (:use [clango.filters :only (deffilter)])
  (:require [clojure.core :as core]
            [clojure.string :as str]))

(deffilter add [:int x :int y]
  (+ x y))

(deffilter addslashes [:string s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "'" "\\'")
      (str/replace "\"" "\\\"")))

(deffilter capfirst [:string s]
  (str/capitalize s))

(deffilter center [:string s :int width]
  (let [lead (int (/ (- width (count s)) 2))
        padding (apply str (repeat lead " "))]
    (if (odd? width)
      (apply str padding s padding " ")
      (apply str padding s padding))))

(deffilter cut [:string s :string to-cut]
  (str/replace s to-cut ""))

(deffilter default [:string s :string df]
  (if (str/blank? s)
    df
    s))

(deffilter default-if-none [:string s :string df]
  (if s
    s
    df))

(deffilter divisibleby [:int num :int div]
  (zero? (mod num div)))

(deffilter filesizeformat [:int size]
  (let [abbrevs ["B" "kB" "MB" "GB" "TB"]
        sizes (take (count abbrevs) (iterate #(/ % 1000.0) size))
        zip (map (fn [x y] [x y]) sizes abbrevs)
        [sz abr] (core/first (drop-while #(> (core/first %) 1000) zip))]
    (if (integer? sz)
      (format "%d %s" sz abr)
      (format "%.1f %s" sz abr))))

(deffilter first [s]
  (core/first s))

(deffilter fix-ampersands [:string s]
  (str/replace s #"&(?!\p{Alpha}+;)" "&amp;"))

(deffilter floatformat
  ([:double n]
     (floatformat n -1))
  ([:double n :int p]
     (let [precision (if (and (neg? p) (== (int n) n))
                       0
                       (Math/abs p))]
       (format (str "%." precision "f") n))))

(deffilter join
  ([x]
     (str/join x))
  ([x :string s]
     (str/join s x)))

(deffilter last [x]
  (core/last x))

(deffilter length [x]
  (str (count x)))

(deffilter limit [x :int n]
  (take n x))

(deffilter reverse [x]
  (core/reverse x))

(deffilter skip [x :int n]
  (drop n x))

(deffilter upper [:string s]
  (str/upper-case s))

(deffilter wordcount [:string s]
  (count (str/split s #"\W+")))

(deffilter yesno [x :string args]
  (let [args (str/split args #",")]
    (cond (and (nil? x) (= (count args) 3)) (nth args 2)
          x (core/first args)
          :else (second args))))
