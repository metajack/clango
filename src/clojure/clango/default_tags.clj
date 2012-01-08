(ns clango.default-tags
  (:require [clango.tags :as tags]
            [clango.var :as var]
            [clango.util :as util]))

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
  (let [expr (var/process-var (first tree) ctx)
        parts (if expr (:true-parts data) (:false-parts data))]
    ["" (concat parts stack)]))

(defn compile-for [tree stack]
  (let [[for-parts new-stack] (tags/collect-until #{"empty" "endfor"} stack)
        [_ tag-name & _] (first new-stack)]
    (if (= tag-name "empty")
      (let [[empty-parts new-stack] (tags/collect-until "endfor" (rest new-stack))]
        [(list :block "for"
               tree
               {:for-parts for-parts :empty-parts empty-parts})
         (rest new-stack)])
      [(list :block "for"
             tree
             {:for-parts for-parts :empty-parts []})
       (rest new-stack)])))

(defn- parse-for-tree [[ivar in coll]]
  (cond
   (not= in '(:var "in")) (throw (Exception. (str "Expected 'in' but got: " in)))
   (not (string? (second ivar))) (throw (Exception. (str "Iterator name must be simple.")))
   :else [(second ivar) coll]))

(defn render-for-parts [ivar coll for-parts ctx]
  (let [len (count coll)
        parent-forloop (:forloop ctx)]
    (loop [counter 1
           [c & cs] coll
           acc []]
      (let [forloop {:counter counter
                     :counter0 (dec counter)
                     :revcounter (- (inc len) counter)
                     :revcounter0 (- len counter)
                     :first (= counter 1)
                     :last (= counter len)
                     :parentloop parent-forloop}
            nctx (assoc ctx
                   :forloop forloop
                   (keyword ivar) c)]
        (if (seq cs)
          (recur (inc counter)
                 cs
                 (into acc (cons nctx for-parts)))
          (into acc (concat [nctx] for-parts [ctx])))))))

(defn render-for [ctx [tree data] stack]
  (let [[ivar coll-var] (parse-for-tree tree)
        coll (var/process-var coll-var ctx)
        parts (if (seq coll)
                (render-for-parts ivar coll (:for-parts data) ctx)
                (:empty-parts data))]
    ["" (concat parts stack)]))

(defn compile-include [tree stack]
  [(list :block "include" tree {}) stack])

(defn render-include [ctx [tree data] stack]
  (let [template-name (var/process-arg (first tree) ctx)
        template (util/get-template template-name ctx)]
    ["" (concat (drop 1 template) stack)]))
