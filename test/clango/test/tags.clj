(ns clango.test.tags
  (:use [clojure.test]
        [clango.test.core :only [deftest-template render]])
  (:require [clango.parser :as parser]
            [clango.core :as clango]))

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

(deftest unknown-tag
  (is (thrown-with-msg? Exception #"^Unknown tag:"
        (render "{% foo %}" {}))))