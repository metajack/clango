(ns clango.var
  (:require [clango.filters :as filters]
            [clango.util :as util]
            [clojure.string :as str]))

(defn- translate-sq-dq [[c & _ :as s]]
  (if (= c \')
    (-> s
        (str/replace "\\'" "'")
        (str/replace #"^'|'$" "\""))
    s))

(defn process-arg [arg context]
  (if (seq? arg)
    (read-string (translate-sq-dq (second arg)))
    (util/lookup context (symbol arg))))

(defn value-of [form context]
  (if-let [[kind val] form]
    (if (= :var kind)
      (cond
       (= val "true") true
       (= val "false") false
       :else (util/lookup context val))
      (read-string (translate-sq-dq val)))
    (util/lookup context form)))

(defn- apply-filter [filter input param context]
  (try
    (if param
      (filter input (process-arg param context))
      (filter input))
    (catch Exception e "")))

(defn process-var [[_ ident & filters] context]
  (reduce
   (fn [acc [_ name param]]
     (apply-filter (filters/filter-for-name name) acc param context))
   (util/lookup context ident)
   filters))

