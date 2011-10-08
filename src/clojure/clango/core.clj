(ns clango.core
  (:require [clango.parser :as parser]
            [clango.filters :as filters]))

(def ^:private my-ns *ns*)

(defn- filter-for-name [name]
  (ns-resolve 'clango.filters (symbol name)))

(defn- lookup [ident context]
  (if (seq? ident)
    (let [[_ a b] ident]
      ((keyword b) (lookup a context)))
    ((keyword ident) context)))

(defn- render-raw [_ [text]]
  text)

(defn- render-var [context [ident & filters]]
  (let [base (lookup ident context)]
    (if (seq? filters)
      (loop [[f & fs] filters
             res base]
        (let [[_ n p] f
              flt (filter-for-name n)
              new-res (str (if p
                             (flt res p)
                             (flt res)))]
          (if fs
            (recur fs new-res)
            new-res)))
      base)))

(defn- render-part [[type & parts] context]
  (let [f @(ns-resolve my-ns (symbol (str "render-" (name type))))]
    (f context parts)))

(defn render [[_ & parts] context]
  (let [rendered (map #(render-part % context) parts)]
    (apply str rendered)))


