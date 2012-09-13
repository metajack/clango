(ns clango.util
  (:require [clojure.string :as string]
            [clango.parser :as parser])
  (:import [java.util Calendar]
           [java.text SimpleDateFormat]))

;; for dot lookups try keyword, string, then numeric key
(defn lookup [context ident]
  (if (seq? ident)
    (if-let [[_ a b] ident]
      (if-let [x (get (lookup context a) (keyword b))]
        x
        (if-let [x (get (lookup context a) b)]
          x
          (when-let [n (try (Long/valueOf b) (catch Exception e))]
            (get (lookup context a) n))))
      (recur context (second ident)))
    (let [res1 ((keyword ident) context)
          res2 ((keyword (string/replace ident #"_" "-")) context)
          maybe-deref #(try @% (catch Exception e %))]
      (or (maybe-deref res1) (maybe-deref res2)))))

(defn get-template [name ctx]
  (if-let [store (:clango.core/template-store ctx)]
    (store name)))

(defn formatter-split [fmt]
  (lazy-seq
   (when-let [s (seq fmt)]
     (when-let [[a b & s] s]
       (cond
        (and a b (= a \%)) (cons (str a b) (formatter-split s))
        (and a b) (cons (str a) (formatter-split (cons b s)))
        a (cons (str a) nil)
        :else nil)))))

(defn trans-format [format]
  (for [f (formatter-split format)]
    (case f
      "%A" "aa"
      "%b" "MMM"
      "%c" "yyyy-MM-dd'T'HH:mm:ss"
      "%d" "dd"
      "%D" "E"
      "%F" "MMMM"
      "%g" "h"
      "%G" "H"
      "%h" "hh"
      "%H" "HH"
      "%i" "mm"
      "%j" "d"
      "%l" "EEEE"
      "%m" "MM"
      "%M" "MMM"
      "%n" "M"
      "%O" "Z"
      "%r" "E, d MMM yyyy HH:mm:ss Z"
      "%s" "ss"
      "%T" "zzz"
      "%U" "S"
      "%W" "w"
      "%y" "yy"
      "%Y" "yyyy"
      "%z" "D"
      (if (= f "'") "''" (str "'" f "'")))))

(defn date-format [date format]
  (let [datestr (fn [s] (-> (SimpleDateFormat. s) (.format date)))
        pieces (for [f (formatter-split format)]
                 (case f
                   "%A" (datestr "aa")
                   "%b" (string/lower-case (datestr "MMM"))
                   ;; separate timezone hour and minute by ":" to
                   ;; conform to IETF restricted subset of ISO8601,
                   ;; which is used in Atom
                   "%c" (let [tz (datestr "Z")
                              h (.substring tz 0 3)
                              m (.substring tz 3)]
                          (str (datestr "yyyy-MM-dd'T'HH:mm:ss") h ":" m))
                   "%d" (datestr "dd")
                   "%D" (datestr "E")
                   "%F" (datestr "MMMM")
                   "%g" (datestr "h")
                   "%G" (datestr "H")
                   "%h" (datestr "hh")
                   "%H" (datestr "HH")
                   "%i" (datestr "mm")
                   "%j" (datestr "d")
                   "%l" (datestr "EEEE")
                   "%m" (datestr "MM")
                   "%M" (datestr "MMM")
                   "%n" (datestr "M")
                   "%O" (datestr "Z")
                   "%r" (datestr "E, d MMM yyyy HH:mm:ss Z")
                   "%s" (datestr "ss")
                   "%T" (datestr "zzz")
                   "%U" (str (int (/ (.getTime date) 1000)))
                   "%w" (str (mod (inc (Long/valueOf (datestr "F"))) 7))
                   "%W" (datestr "w")
                   "%y" (datestr "yy")
                   "%Y" (datestr "yyyy")
                   "%z" (datestr "D")
                   f))]
    (apply str pieces)))

(defn date-parse [s format]
  (let [fmt (apply str (trans-format format))]
    (-> (SimpleDateFormat. fmt)
        (.parse s))))
