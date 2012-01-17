(ns clango.core
  (:require [clojure.string :as str]
            [clango.parser :as parser]
            [clango.var :as var]
            [clango.filters :as filters]
            [clango.tags :as tags]
            [clango.util :as util]
            clango.default-filters
            clango.default-tags))

(def ^:private my-ns *ns*)

(def ^:dynamic *context* {})

(defn- render-part-raw [[text] stack _]
  [text stack])

(defn- render-part-var [v stack context]
  [(str (var/process-var (cons :var v) context)) stack])

(defn- render-part-block [[name & parts] stack context]
  (let [tag (tags/tag-for-name :render name)]
    (tag context parts stack)))

(defn- render-part-template [parts stack context]
  (let [parts (tags/compile parts)]
    ["" (concat parts stack)]))

(defn- render-part [[type & parts] stack context]
  (let [f @(ns-resolve my-ns (symbol (str "render-part-" (name type))))]
    (f parts stack context)))

(defn render [[_ & parts] & {:keys [context] :or {context *context*}}]
  (let [parts (tags/compile parts)]
    (loop [stack parts
           ctx context
           acc []]
      (if (seq stack)
        (let [[part & next-parts] stack]
          (if (map? part)
            (recur next-parts part acc)
            (let [[res new-stack] (render-part part next-parts ctx)]
              (recur new-stack ctx (conj acc res)))))
        (apply str acc)))))

(defn render-template [name & {:keys [context store] :or {context *context*}}]
  (let [template (store name)
        context (assoc context ::template-store store)]
    (render template :context context)))

(defn render-string [s & {:keys [context store] :or {context *context*}}]
  (let [template (parser/parse s)
        context (assoc context ::template-store store)]
    (render template :context context)))
