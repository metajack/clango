(ns clango.util)

(defn lookup [context ident]
  (if (seq? ident)
    (let [[_ a b] ident]
      ((keyword b) (lookup context a)))
    ((keyword ident) context)))
