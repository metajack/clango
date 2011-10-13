(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

(defn coerce-int [s]
  (if (string? s)
    (Integer/valueOf s)
    (int s)))

(defn coerce-double [s]
  (if (string? s)
    (Double/valueOf s)
    (double s)))

(defn coerce-string [s]
  (when s
    (if (string? s)
      s
      (String/valueOf s))))

(defn coerce-pass [s]
  s)

(defn- transform-fdecls [fdecls]
  (loop [[fdecl & fdecls] fdecls
         acc ()]
    (let [arglist (core/first fdecl)
          body (rest fdecl)]
      (when (not (vector? arglist))
        (throw (Exception. "Missing argument list")))
      (when (not (even? (count arglist)))
        (throw (Exception. "Must have even number of arguments")))
      (when (not (or (= (count arglist) 2) (= (count arglist) 4)))
        (throw (Exception. "Filters take only one or two parameters")))
      
      (let [bindpairs (for [[coersion sym] (partition 2 arglist)]
                        [sym (list (symbol (str "clango.filters/coerce-" (name coersion))) sym)])
            bindings (vec (apply concat bindpairs))
            fnbody `(~(vec (map core/first bindpairs))
                     (let ~bindings
                       ~@body))]
        (if fdecls
          (recur fdecls (conj acc fnbody))
          (conj acc fnbody))))))

;; write filters as
;; (deffilter foo [:string x]
;;   ...)
;; and x will be automatically coerced to string in the body
(defmacro deffilter [name & fdecl]
  (let [docstring (if (string? (core/first fdecl))
                    (core/first fdecl)
                    "")
        fdecl (if (string? (core/first fdecl))
                (next fdecl)
                fdecl)
        fdecls (if (vector? (core/first fdecl))
                (list fdecl)
                fdecl)
        fdecls (transform-fdecls fdecls)]
    `(defn ~name
       ~docstring
       ~@fdecls)))

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

(deffilter first [:pass s]
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
  ([:pass x]
     (str/join x))
  ([:pass x :string s]
     (str/join s x)))

(deffilter last [:pass x]
  (core/last x))

(deffilter length [:pass x]
  (count x))

(deffilter upper [:string s]
  (str/upper-case s))

(deffilter wordcount [:string s]
  (count (str/split s #"\W+")))

(deffilter yesno [:pass x :string args]
  (let [args (str/split args #",")]
    (cond (and (nil? x) (= (count args) 3)) (nth args 2)
          x (core/first args)
          :else (second args))))
