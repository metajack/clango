(ns clango.default-tags
  (:require [clango.tags :as tags]))

(defn compile-if [tree stack]
  (let [[true-parts new-stack] (tags/collect-until #{"else" "endif"} stack)]
    (let [[_ tag-name & _] (first new-stack)]
      (if (= tag-name "else")
        (let [[false-parts new-stack] (tags/collect-until "endif" (rest new-stack))]
          [(list :block "if"
                 tree
                 {:true-parts true-parts :false-parts false-parts})
           (rest new-stack)])
        [(list :block "if"
               tree
               {:true-parts true-parts :false-parts []})
         (rest new-stack)]))))

(defn render-if [ctx [tree data] stack]
  (let [expr (tags/lookup ctx (first tree))
        parts (if expr (:true-parts data) (:false-parts data))]
    ["" (concat parts stack)]))