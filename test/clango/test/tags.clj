(ns clango.test.tags
  (:use [clojure.test]
        [clango.test.core :only [deftest-template render]])
  (:require [clango.parser :as parser]
            [clango.core :as clango]
            [clango.stores :as stores]))

(def store
  (stores/map-store
   {"parent.html.ctl"
    "{% block content %}{% endblock %}"
    "child.html.ctl"
    "{% extends 'parent.html.ctl' %}{% block content %}asdf{% endblock %}"
    "include.html.ctl"
    "asdf"}))

(deftest-template if-tag-false
  {:foo false}
  "{% if foo %}asdf{% endif %}"
  "")

(deftest-template if-tag-true
  {:foo true}
  "{% if foo %}asdf{% endif %}"
  "asdf")

(deftest-template if-tag-else-1
  {:foo false}
  "{% if foo %}asdf{% else %}jkl;{% endif %}"
  "jkl;")

(deftest-template if-tag-else-2
  {:foo true}
  "{% if foo %}asdf{% else %}jkl;{% endif %}"
  "asdf")

(deftest-template nested-if-tag-1
  {:foo true :bar false}
  "{% if foo %}as{% if bar %}df{% else %}l;{% endif %}{% else %}jk{% endif %}"
  "asl;")

(deftest-template nested-if-tag-2
  {:foo true :bar true}
  "{% if foo %}as{% if bar %}df{% else %}l;{% endif %}{% else %}jk{% endif %}"
  "asdf")

(deftest-template nested-if-tag-3
  {:foo false :bar false}
  "{% if foo %}as{% if bar %}df{% else %}l;{% endif %}{% else %}jk{% endif %}"
  "jk")

(deftest-template if-nested-var
  {:foo {:bar true}}
  "{% if foo.bar %}asdf{% else %}jkl;{% endif %}"
  "asdf")

(deftest unknown-tag
  (is (thrown-with-msg? Exception #"^Unknown tag:"
        (render "{% foo %}" {}))))

(defmacro deftest-forloop [name body output]
  `(deftest-template
     ~name
     {:foo ["a" "b" "c"]}
     (str "{% for f in foo %}" ~body "{% endfor %}")
     ~output))

(deftest-template for-loop-empty
  {:foo []}
  "{% for f in foo %}asdf{% empty %}jkl;{% endfor %}"
  "jkl;")

(deftest-forloop for-loop
  "{{ f }}."
  "a.b.c.")

(deftest-forloop for-loop-counter
  "{{ forloop.counter }}."
  "1.2.3.")

(deftest-forloop for-loop-counter0
  "{{ forloop.counter0 }}."
  "0.1.2.")

(deftest-forloop for-loop-revcounter
  "{{ forloop.revcounter }}."
  "3.2.1.")

(deftest-forloop for-loop-revcounter0
  "{{ forloop.revcounter0 }}."
  "2.1.0.")

(deftest-forloop for-loop-first
  "{% if forloop.first %}.{% endif %}{{ f }}."
  ".a.b.c.")

(deftest-forloop for-loop-last
  "{{ f }}{% if forloop.last %}{% else %}.{% endif %}"
  "a.b.c")

(deftest-forloop for-loop-parent
  "{% for g in foo %}{{ f }}-{{ forloop.parentloop.counter }}-{{ g }}.{% endfor %}."
  "a-1-a.a-1-b.a-1-c..b-2-a.b-2-b.b-2-c..c-3-a.c-3-b.c-3-c..")

(deftest-template for-loop-reversed
  {:foo [1 2 3]}
  "{% for f in foo|reverse %}{{ f }}.{% endfor %}"
  "3.2.1.")

(deftest-template for-loop-limit
  {:foo [1 2 3]}
  "{% for f in foo|limit:2 %}{{ f }}.{% endfor %}"
  "1.2.")

(deftest-template basic-include
  {:clango.core/template-store store}
  "{% include 'include.html.ctl' %}"
  "asdf")
