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

(defmacro if-op-binary [sym f tree else]
  `(let [[l# r#] (split-with #(not= % [:sym ~sym]) ~tree)]
     (if (seq r#)
       (list ~f (resolve-if-tree l#) (resolve-if-tree (rest r#)))
       ~else)))

(defmacro if-op-unary [sym f tree else]
  `(if (= [:sym ~sym] (first ~tree))
     (resolve-if-tree (concat [(list ~f (second ~tree))] (drop 2 ~tree)))
     ~else))

(defn resolve-if-tree [[h & t :as tree]]
  (if (seq t)
    (if-op-binary
     "==" '= tree
     (if-op-binary
      "!=" 'not= tree
      (if-op-binary
       "<" '< tree
       (if-op-binary
        ">" '> tree
        (if-op-binary
         "<=" '<= tree
         (if-op-binary
          ">=" '>= tree
          (if-op-binary
           "in" #(>= (.indexOf %2 %1) 0) tree
           (if-op-binary
            "notin" #(< (.indexOf %2 %1) 0) tree
            (if-op-unary
             "not" 'not tree
             (if-op-binary
              "and" 'and tree
              (if-op-binary
               "or" 'or tree
               (throw (Exception. (str "Unexpected expression: " (apply str (interpose " " tree))))))))))))))))
    (if (sequential? h)
      (if (= :sym (first h))
        (throw (Exception. (str "Unexpected expression: " h)))
        h)
      h)))

(defn if-combine-not-in [tree]
  (loop [[h & t] tree
         acc []]
    (if (seq t)
      (if (and (= [:sym "not"] h) (= [:sym "in"] (first t)))
        (recur (rest t) (conj acc [:sym "notin"]))
        (recur t (conj acc h)))
      (conj acc h))))

(defn render-if [ctx [tree data] stack]
  (let [tree (for [n tree]
               (cond
                (= :sym (first n)) n
                (= [:var "in"] n) [:sym "in"]
                (= [:var "not"] n) [:sym "not"]
                (= [:var "and"] n) [:sym "and"]
                (= [:var "or"] n) [:sym "or"]
                :else (let [v (var/value-of n ctx)]
                        (cond
                         (sequential? v) (seq v)
                         (= v "") false
                         :else v))))
        tree (if-combine-not-in tree)]
    (let [expr (eval (resolve-if-tree tree))
          parts (if expr (:true-parts data) (:false-parts data))]
      ["" (concat parts stack)])))

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
    ["" (concat [template] stack)]))

(defn compile-extends [tree stack]
  (let [[parts _] (tags/collect-until #{} stack)
        block-nodes (filter #(= (take 2 %) [:block "block"]) parts)
        blocks (into {} (for [bnode block-nodes]
                          (let [bname (keyword (second (first (nth bnode 2))))
                                bbody (:contents (nth bnode 3))]
                            [bname [bbody]])))]
    [(list :block "extends" tree {:blocks blocks}) []]))

(defn- add-blocks [old new]
  (reduce
   (fn [acc [k v]]
     (let [oldv (get acc k)]
       (assoc acc k (concat oldv v))))
   (if old old {})
   new))

(defn render-extends [ctx [tree data] stack]
  (let [template-name (var/process-arg (first tree) ctx)
        template (util/get-template template-name ctx)
        root (empty? (filter #(= (take 2 %) [:block "extends"]) (rest template)))
        blocks (add-blocks (::extends-blocks ctx) (:blocks data))
        nctx (assoc ctx ::extends-root root ::extends-blocks blocks)]
    ["" (concat [nctx template] stack)]))

(defn compile-block [tree stack]
  (let [[contents new-stack] (tags/collect-until "endblock" stack)]
    [(list :block "block" tree {:contents contents}) (rest new-stack)]))

(defn- expand-block [block]
  (loop [[b & bs] (first block)
         acc []]
    (let [body (if (= b [:var [:. "block" "super"]])
                 (expand-block (rest block))
                 [b])]
      (if (seq bs)
        (recur bs (into acc body))
        (into acc body)))))

(defn render-block [ctx [tree data] stack]
  (if (::extends-root ctx)
    (let [bname (keyword (second (first tree)))
          blocks (::extends-blocks ctx)
          block (concat (get blocks bname []) [(:contents data)])]
      ["" (concat (expand-block block) stack)])
    ["" stack]))

(defn compile-with [tree stack]
  (let [[contents new-stack] (tags/collect-until "endwith" stack)]
    [(list :block "with" tree {:contents contents}) (rest new-stack)]))

(defn render-with [ctx [tree data] stack]
  (let [pairs (filter #(not= % [[:sym ","]])
                      (partition-by #(= % [:sym ","]) tree))
        pairs (map (fn [[left eq right]]
                     (cond
                      (not= eq [:sym "="]) (throw (Exception. (str "Unexpected symbol " (second eq))))
                      (not= :var (first left)) (throw (Exception. (str "Expected var but got " left)))
                      :else [(keyword (second left)) (var/value-of right ctx)]))
                   pairs)
        new-ctx (into ctx pairs)]
    ["" (concat [new-ctx] (:contents data) [ctx] stack)]))