(ns clango.parser
  (:import [org.antlr.runtime ANTLRStringStream CommonTokenStream Token]
           [org.antlr.runtime RecognitionException MismatchedTokenException]
           [org.antlr.runtime NoViableAltException]
           [clango.antlr TemplateLexer TemplateParser]))

(defn lex [s]
  (let [lexer (-> s (ANTLRStringStream.) (TemplateLexer.))]
    (loop [token (.nextToken lexer)
           acc []]
      (if (= (.getType token) Token/EOF)
        acc
        (recur (.nextToken lexer) (conj acc [(.getText token) (.getType token)]))))))

(defn ast [node]
  (if (and (zero? (.getChildCount node))
           (.getParent node))
    (.getText node)
    (let [children (map ast (.getChildren node))
          text (.getText node)]
      (if text
        (cons (keyword (.toLowerCase text)) children)
        children))))

(defn token-name [type]
  (if (neg? type)
    "<EOF>"
    (let [names TemplateParser/tokenNames
          name (str "<" (aget names type) ">")]
      (case name
        "<CLOSE_VAR>" "}}"
        "<CLOSE_BLOCK>" "%}"
        "<CLOSE_COMMENT>" "#}"
        "<PIPE>" "|"
        "<COLON>" ":"
        name))))

(defn token-to-string [token]
  (let [name (token-name (.getType token))
        translate? #{"<IDENTIFIER>" "<NUMBER>" "<STRING>"}]
    (if (translate? name)
      (str (.getText token)
           ":"
           (token-name (.getType token)))
      name)))

(defn error-msg [re parser]
  (condp instance? re
    MismatchedTokenException (str "Invalid input at line: "
                                  (.line re)
                                  " col: "
                                  (inc (.charPositionInLine re))
                                  ": expected "
                                  (token-name (.expecting re))
                                  " but got "
                                  (token-to-string (.token re)))
    NoViableAltException (str "Invalid input at line: "
                              (.line re)
                              " col: "
                              (inc (.charPositionInLine re))
                              ": "
                              (if-let [token (.token re)]
                                (token-to-string token)
                                (char (.c re))))
    (str "Unknown exception: " (.toString re))))

(defn parse [s]
  (let [ass (ANTLRStringStream. s)
        tl (TemplateLexer. ass)
        cts (CommonTokenStream. tl)
        tp (TemplateParser. cts)]
    (try
      (-> tp
          (.template)
          (.getTree)
          (ast))
      (catch RecognitionException re
        (throw (Exception. (error-msg re tp))))
      (catch RuntimeException rte
        (let [cause (.getCause rte)]
          (if (instance? NoViableAltException cause)
           (throw (Exception. (error-msg cause tp)))
           (throw rte)))))))
