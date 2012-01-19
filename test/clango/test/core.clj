(ns clango.test.core
  (:use [clojure.test])
  (:require [clango.parser :as parser]
            [clango.core :as clango])
  (:import [org.antlr.runtime MismatchedTokenException]
           [clango.antlr TemplateParser]))

(defmacro deftest-template [name context template output]
  `(deftest ~name
     (let [t# (if (string? ~template)
                (parser/parse ~template)
                ~template)]
       (is (= ~output (clango/render t# :context ~context))
           "Tempalte output didn't match given output."))))

(defn render [s ctx]
  (clango/render (parser/parse s) :context ctx))

(deftest-template raw-render
  {}
  "asdf"
  "asdf")

(deftest-template ident-dot-number
  {:foo ["zero" "one" "two"]}
  "{{ foo.1 }}"
  "one")

(deftest bad-var
  (is (thrown? Exception (render "{{foo" {}))))
