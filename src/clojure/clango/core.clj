(ns clango.core
  (:require [clojure.string :as str]
            [clango.parser :as parser]
            [clango.filters :as filters]))

(def ^:private my-ns *ns*)

(defn- translate-sq-dq [[c & _ :as s]]
  (if (= c \')
    (-> s
        (str/replace "\\'" "'")
        (str/replace #"^'|'$" "\""))
    s))

(defn- lookup [ident context]
  (if (seq? ident)
    (let [[_ a b] ident]
      ((keyword b) (lookup a context)))
    ((keyword ident) context)))

(defn- convert-arg [arg context]
  (if (seq? arg)
    (read-string (translate-sq-dq (second arg)))
    (lookup (symbol arg) context)))

(defn- filter-for-name [name]
  (ns-resolve 'clango.filters (symbol (str/replace name "_" "-"))))

(defn- render-raw [_ [text]]
  text)

(defn- apply-filter [filter input param context]
  (try
    (str (if param
           (filter input (convert-arg param context))
           (filter input)))
    (catch Exception e "")))

(defn- render-var [context [ident & filters]]
  (let [base (lookup ident context)]
    (if (seq? filters)
      (loop [[f & fs] filters
             res base]
        (let [[_ n p] f
              flt (filter-for-name n)
              new-res (apply-filter flt res p context)]
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


