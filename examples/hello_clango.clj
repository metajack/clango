(ns clango.examples.hello-clango
  (require [clango.core :as clango]))

(def template
  "{{ greeting|captitalize }}, {{ object }}!")

(def context
  {:greeting "hello"
   :object "world"})

(defn greet []
  (let [output (clango/render-string template :context context)]
    (println output)))
