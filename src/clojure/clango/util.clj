(ns clango.util
  (:require [clango.parser :as parser]))

(defn lookup [context ident]
  (if (seq? ident)
    (let [[_ a b] ident]
      ((keyword b) (lookup context a)))
    ((keyword ident) context)))

(defn get-template [name ctx]
  (if-let [store (:clango.core/template-store ctx)]
    (if-let [t (store name)]
      (parser/parse t))))
