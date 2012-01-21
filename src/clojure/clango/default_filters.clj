(ns clango.default-filters
  (:refer-clojure :exclude [first last reverse])
  (:use [clango.filters :only (deffilter)])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clango.util :as util]
            [net.cgrand.enlive-html :as html])
  (:import [java.io StringReader]
           [java.util.regex Pattern]
           [java.net URLEncoder]))

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

(deffilter date [d :string format]
  (util/date-format d format))

(deffilter dateparse [s :string format]
  (util/date-parse s format))

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

(deffilter truncatewords [x :int n]
  (->> x
       (StringReader.)
       (html/html-resource)
       (html/texts)
       (apply str)
       (re-find (Pattern/compile (str "(\\W*\\w+\\W*){0," n "}")))
       (first)
       (str/trim)))

(deffilter upper [:string s]
  (str/upper-case s))

(deffilter urlencode [:string s]
  (URLEncoder/encode s "UTF-8"))

(deffilter wordcount [:string s]
  (count (str/split s #"\W+")))

(deffilter yesno [x :string args]
  (let [args (str/split args #",")
        answers (if (= 3 (count args))
                  (zipmap [:yes :no :maybe] args)
                  (zipmap [:yes :no :maybe] (concat args [(second args)])))]
    (cond
     (nil? x) (:maybe answers)
     (or (not x)
         (= "" x)
         (when (or (sequential? x)
                   (associative? x))
           (seq x))) (:no answers)
     :else (:yes answers))))
