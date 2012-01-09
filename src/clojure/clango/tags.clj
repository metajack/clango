(ns clango.tags
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [clango.util :as util]))

(def lookup util/lookup)

(defn tag-for-name [typ n]
  (let [tag-sym (symbol (str (name typ) "-" (str/replace n "_" "-")))]
    (if-let [tag (ns-resolve (find-ns 'clango.default-tags) tag-sym)]
      tag
      (throw (Exception. (str "Unknown tag: " n))))))

(defn- compile-once [stack]
  (if (seq stack)
    (let [[p & parts] stack
          [type name & tree] p]
      (if (= type :block)
        (let [compiler (tag-for-name :compile name)
              [result new-stack] (compiler tree parts)]
          [[result] new-stack])
        [[p] parts]))
    [[] []]))

(defn compile [stack]
  (loop [stack stack
         acc []]
    (if (seq stack)
      (let [[compiled new-stack] (compile-once stack)]
        (recur new-stack (into acc compiled)))
      acc)))

(defn collect-until [tag-names stack]
  (let [tag-names (if (not (set? tag-names))
                    (if tag-names #{tag-names} #{})
                    tag-names)]
    (loop [stack stack
           acc []]
      (if (seq stack)
        (let [[type tag-name & _] (first stack)]
          (if (and (= type :block) (tag-names tag-name))
            [acc stack]
            (let [[compiled new-stack] (compile-once stack)]
              (recur new-stack (into acc compiled)))))
        [acc []]))))

