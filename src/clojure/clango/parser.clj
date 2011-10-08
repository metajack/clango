(ns clango.parser
  (:import [org.antlr.runtime ANTLRStringStream CommonTokenStream Token]
           [clango.antlr TemplateLexer TemplateParser]))

(defn lex [s]
  (let [lexer (-> s (ANTLRStringStream.) (TemplateLexer.))]
    (loop [token (.nextToken lexer)
           acc []]
      (if (= (.getType token) Token/EOF)
        acc
        (recur (.nextToken lexer) (conj acc [(.getText token) (.getType token)]))))))

(defn ast [node]
;  (println (bean node))
  (if (zero? (.getChildCount node))
    (.getText node)
    (let [children (map ast (.getChildren node))
          text (.getText node)]
      (if text
        (cons (keyword (.toLowerCase  text)) children)
        children))))

(defn parse [s]
  (-> s
      (ANTLRStringStream.)
      (TemplateLexer.)
      (CommonTokenStream.)
      (TemplateParser.)
      (.template)
      (.getTree)
      (ast)))