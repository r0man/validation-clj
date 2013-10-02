(defproject validation-clj "0.5.7-SNAPSHOT"
  :description "A validation library for Clojure."
  :min-lein-version "2.0.0"
  :url "http://github.com/r0man/validation-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :lein-release {:deploy-via :clojars}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [slingshot "0.10.3"]
                 [geo-clj "0.3.2"]])
