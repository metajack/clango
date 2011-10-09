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

(deftest-template filtet-param-identifier
  {:foo 1 :bar 2}
  "{{ foo|add:bar }}"
  "3")

(deftest-template first-filter-vector
  {:foo [1 2 3]}
  "{{ foo|first }}"
  "1")

(deftest-template first-filter-string
  {:foo "asdf"}
  "{{ foo|first }}"
  "a")

(deftest-template first-filter-error
  {:foo 1}
  "{{ foo|first }}"
  "")

(deftest-template last-filter
  {:foo [1 2 3]}
  "{{ foo|last }}"
  "3")

(deftest-template join-filter
  {:foo ["one" "two" "three"]}
  "{{ foo|join:',' }}"
  "one,two,three")

(deftest-template join-filter-no-param
  {:foo ["one" "two" "three"]}
  "{{ foo|join }}"
  "onetwothree")

(deftest-template add-filter-integers
  {:foo 42}
  "{{ foo|add:10 }}"
  "52")

(deftest-template add-filter-floats
  {:foo 42.5}
  "{{ foo|add:10.5 }}"
  "52")

(deftest-template addslashes-filter
  {:foo "asdf\\asdf'asdf\"asdf"}
  "{{ foo|addslashes }}"
  "asdf\\\\asdf\\'asdf\\\"asdf")

(deftest-template center-filter
  {:foo "asdf"}
  "{{ foo|center:10 }}"
  "   asdf   ")

(deftest-template cut-filter
  {:foo "asdf asdf"}
  "{{ foo|cut:' ' }}"
  "asdfasdf")

(deftest-template default-filter
  {:foo ""}
  "{{ foo|default:'asdf' }}"
  "asdf")

(deftest-template default-filter-pass
  {:foo "asdf"}
  "{{ foo|default:'jkl;' }}"
  "asdf")

(deftest-template default-if-none-filter
  {:foo nil}
  "{{ foo|default_if_none:'asdf' }}"
  "asdf")

(deftest-template default-if-none-filter-pass
  {:foo ""}
  "{{ foo|default_if_none:'asdf' }}"
  "")

(deftest-template divisibleby-filter
  {:foo 9}
  "{{ foo|divisibleby:3 }}"
  "true")

(deftest-template filesizeformat-filter
  {:foo 1234}
  "{{ foo|filesizeformat }}"
  "1.2 kB")

(deftest-template fix-ampersands-filter
  {:foo "as&df&gt;"}
  "{{ foo|fix_ampersands }}"
  "as&amp;df&gt;")
