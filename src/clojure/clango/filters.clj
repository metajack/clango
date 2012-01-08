(ns clango.filters
  (:refer-clojure :exclude [first last])
  (:require [clojure.core :as core]
            [clojure.string :as str]
            [clojure.pprint :as pp]))



(defn filter-for-name [name]
  (if-let [filter (ns-resolve 'clango.default-filters
                              (symbol (str (str/replace name "_" "-"))))]
    filter
    (throw (Exception. (str "Unknown filter: " name)))))

(defn coerce-int [s]
  `(if (string? ~s)
     (Integer/valueOf ~s)
     (int ~s)))

(defn coerce-double [s]
  `(if (string? ~s)
     (Double/valueOf ~s)
     (double ~s)))

(defn coerce-string [s]
  `(when ~s
     (if (string? ~s)
       ~s
       (String/valueOf ~s))))

(defn coerce-pass [s]
  s)

(defn- expand [args]
  (loop [[a & as] args
         pos 0
         acc []]
    (if (seq as)
      (cond
       (and (even? pos) (keyword? a)) (recur as (inc pos) (conj acc a))
       (even? pos) (recur as pos (conj acc :pass a))
       (and (odd? pos) (keyword? a)) (throw (Exception. "Type specifier in wrong place"))
       :else (recur as (inc pos) (conj acc a)))
      (cond
       (and (odd? pos) (symbol? a)) (conj acc a)
       (and (even? pos) (symbol? a)) (conj acc :pass a)
       :else (throw (Exception. "Arglist incomplete"))))))

(defn- transform-fdecls [fdecls]
  (loop [[fdecl & fdecls] fdecls
         acc ()]
    (let [arglist (core/first fdecl)
          body (rest fdecl)]
      (when (not (vector? arglist))
        (throw (Exception. "Missing argument list")))
      (when (> (count arglist) 4)
        (throw (Exception. "Filters take only one or two parameters")))
      (let [bindpairs (for [[coersion sym] (partition 2 (expand arglist))]
                        [sym ((resolve (symbol (str "clango.filters/coerce-"
                                                    (name coersion))))
                              sym)])
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
                    (list (core/first fdecl))
                    ())
        fdecl (if (string? (core/first fdecl))
                (next fdecl)
                fdecl)
        fdecls (if (vector? (core/first fdecl))
                (list fdecl)
                fdecl)
        fdecls (transform-fdecls fdecls)]
    `(defn ~name
       ~@docstring
       ~@fdecls)))

