(ns clango.test.tags
  (:use [clojure.test]
        [clango.test.core :only [deftest-template render]])
  (:require [clango.parser :as parser]
            [clango.core :as clango]
            [clango.stores :as stores]))

(def store
  (stores/map-store
   {"parent.html.ctl"
    "{% block content %}...{% endblock %}"
    "child.html.ctl"
    "{% extends 'parent.html.ctl' %}{% block content %}asdf{% endblock %}"
    "child-nothing.html.ctl"
    "{% extends 'parent.html.ctl' %}"
    "child-super.html.ctl"
    "{% extends 'parent.html.ctl' %}{% block content %}asdf{{ block.super }}{% endblock %}"
    "child-child.html.ctl"
    "{% extends 'child.html.ctl' %}{% block content %}jkl;{% endblock %}"
    "child-child-super.html.ctl"
    "{% extends 'child-super.html.ctl' %}{% block content %}jkl;{{ block.super }}{% endblock %}"
    "raw-include.html.ctl"
    "asdf"
    "simple-include.html.ctl"
    "{{ foo }}"}))

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

(deftest-template if-op-equalsequals
  {:foo "asdf" :bar "jkl;"}
  "{% if foo == bar %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-op-bangequals
  {:foo "asdf" :bar "jkl;"}
  "{% if foo != bar %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-op-lessthan
  {:foo 1 :bar 2}
  "{% if foo < bar %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-op-greaterthan
  {:foo 1 :bar 2}
  "{% if foo > bar %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-op-lessthanequals
  {:foo 1 :bar 2}
  "{% if foo <= bar %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-op-greaterthanequals
  {:foo 1 :bar 2}
  "{% if foo >= bar %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-op-in
  {:foo "asdf" :bar "jkl;asdfjkl;"}
  "{% if foo in bar %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-op-not
  {:foo false}
  "{% if not foo %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-op-and
  {:foo true :bar false}
  "{% if foo and bar %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-op-or
  {:foo true :bar false}
  "{% if foo or bar %}true{% else %}false{% endif %}"
  "true")

(deftest-template if-and-before-or
  {:foo false :bar true :baz false}
  "{% if foo and bar or baz %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-empty-is-false-1
  {:foo []}
  "{% if foo %}true{% else %}false{% endif %}"
  "false")

(deftest-template if-empty-is-false-2
  {:foo ""}
  "{% if foo %}true{% else %}false{% endif %}"
  "false")

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
  "{% include 'raw-include.html.ctl' %}"
  "asdf")

(deftest-template basic-include
  {:clango.core/template-store store
   :foo "jkl;"}
  "{% include 'simple-include.html.ctl' %}"
  "jkl;")

(deftest-template basic-extends
  {:clango.core/template-store store}
  "{% extends 'raw-include.html.ctl' %}"
  "asdf")

(defmacro deftest-extends [name tmpl-name output]
  `(deftest-template
     ~name
     {:clango.core/template-store store}
     (store ~tmpl-name)
     ~output))

(deftest-extends basic-block
  "child.html.ctl"
  "asdf")

(deftest-extends none-block
  "child-nothing.html.ctl"
  "...")

(deftest-extends super-block
  "child-super.html.ctl"
  "asdf...")

(deftest-extends multi-level-block
  "child-child.html.ctl"
  "jkl;")

(deftest-extends multi-super-block
  "child-child-super.html.ctl"
  "jkl;asdf...")

(deftest-template with-block
  {:foo "asdf" :baz "1234"}
  "{% with foo='jkl;', bar=baz %}{{ foo }}{{ bar }}{% endwith %}{{ foo }}"
  "jkl;1234asdf")
