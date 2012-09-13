(defproject clango "1.0.0-SNAPSHOT"
  :description "Django-style templates for Clojure"
  :url "https://github.com/metajack/clango"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [enlive "1.0.1"]
                 [org.antlr/antlr "3.4"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"])
