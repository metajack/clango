(ns clango.test.core
  (:use [clojure.test])
  (:require [clango.parser :as parser]
            [clango.core :as clango]))

(defmacro deftest-template [name context template output]
  `(deftest ~name
     (let [t# (parser/parse ~template)]
       (is (= ~output (clango/render t# ~context))
           "Tempalte output didn't match given output."))))

(defn render [s ctx]
  (clango/render (parser/parse s) ctx))

(deftest-template raw-render
  {}
  "asdf"
  "asdf")
