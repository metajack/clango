(ns clango.core
  (:require [clojure.string :as str]
            [clango.parser :as parser]
            [clango.filters :as filters]
            [clango.tags :as tags]
            [clango.util :as util]
            clango.default-filters
            clango.default-tags))

(def ^:private my-ns *ns*)

(def ^:dynamic *context* {})

(defn- translate-sq-dq [[c & _ :as s]]
  (if (= c \')
    (-> s
        (str/replace "\\'" "'")
        (str/replace #"^'|'$" "\""))
    s))

(defn- convert-arg [context arg]
  (if (seq? arg)
    (read-string (translate-sq-dq (second arg)))
    (util/lookup context (symbol arg))))

(defn- render-raw [[text] stack _]
  [text stack])

(defn- apply-filter [filter input param context]
  (try
    (str (if param
           (filter input (convert-arg context param))
           (filter input)))
    (catch Exception e "")))

(defn- render-var [[ident & filters] stack context]
  (let [base (util/lookup context ident)]
    (if (seq? filters)
      (loop [[f & fs] filters
             res base]
        (let [[_ n p] f
              flt (filters/filter-for-name n)
              new-res (apply-filter flt res p context)]
          (if fs
            (recur fs new-res)
            [new-res stack])))
      [base stack])))

(defn- render-block [[name & parts] stack context]
  (let [tag (tags/tag-for-name :render name)]
    (tag context parts stack)))

(defn- render-part [[type & parts] stack context]
  (let [f @(ns-resolve my-ns (symbol (str "render-" (name type))))]
    (f parts stack context)))

(defn render [[_ & parts] & {:keys [context] :or {context *context*}}]
  (let [parts (tags/compile parts)]
    (loop [stack parts
           acc []]
      (if (seq stack)
        (let [[res new-stack] (render-part (first stack) (rest stack) context)]
          (recur new-stack (conj acc res)))
        (apply str acc)))))
