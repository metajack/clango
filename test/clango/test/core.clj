(ns clango.test.core
  (:use [clojure.test])
  (:require [clango.parser :as parser]
            [clango.core :as clango]))

(defmacro deftest-template [name context template output]
  `(deftest ~name
     (let [t# (parser/parse ~template)]
       (is (= ~output (clango/render t# ~context))
           "Tempalte output didn't match given output."))))

(deftest-template raw-render
  {}
  "asdf"
  "asdf")

(deftest-template unbound-var-render
  {}
  "{{foo}}"
  "")

(deftest-template simple-var-render
  {:foo "asdf"}
  "{{foo}}"
  "asdf")

(deftest-template dotted-var-render
  {:foo {:bar "asdf"}}
  "{{ foo.bar }}"
  "asdf")

(deftest-template basic-filter-render
  {:foo "asdf"}
  "{{ foo|length }}"
  "4")

(deftest-template double-filter-render
  {:foo "asdf"}
  "{{foo|length|length}}"
  "1")
