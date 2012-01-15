(ns clango.util
  (:require [clojure.string :as str]
            [clango.parser :as parser])
  (:import [java.util Calendar]
           [java.text SimpleDateFormat]))

(defn lookup [context ident]
  (if (seq? ident)
    (if-let [[_ a b] ident]
      ((keyword b) (lookup context a))
      (recur context (second ident)))
    (let [res ((keyword ident) context)]
      (try
        @res
       (catch Exception e res)))))

(defn get-template [name ctx]
  (if-let [store (:clango.core/template-store ctx)]
    (if-let [t (store name)]
      (parser/parse t))))

(defn formatter-split [fmt]
  (lazy-seq
   (when-let [s (seq fmt)]
     (when-let [[a b & s] s]
       (cond
        (and a b (= a \%)) (cons (str a b) (formatter-split s))
        (and a b) (cons (str a) (formatter-split (cons b s)))
        a (cons (str a) nil)
        :else nil)))))

(defn date-format [date format]
  (let [datestr (fn [s] (-> (SimpleDateFormat. s) (.format date)))
        pieces (for [f (formatter-split format)]
                 (case f
                   "%A" (datestr "aa")
                   "%b" (str/lower-case (datestr "MMM"))
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
