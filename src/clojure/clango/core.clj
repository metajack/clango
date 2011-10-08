(ns clango.core
  (:require [clango.parser :as parser]))

(def ^:private my-ns *ns*)

(defn lookup [ident context]
  (if (seq? ident)
    (let [[_ a b] ident]
      ((keyword b) (lookup a context)))
    ((keyword ident) context)))

(defn- render-raw [_ [text]]
  text)

(defn- render-var [context [ident & filters :as everything]]
  (let [base (lookup ident context)]
    base))

(defn- render-part [[type & parts] context]
  (let [f @(ns-resolve my-ns (symbol (str "render-" (name type))))]
    (f context parts)))

(defn render [[_ & parts] context]
  (let [rendered (map #(render-part % context) parts)]
    (apply str rendered)))


