(defproject closure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clj-http "3.10.0"]
                 [slingshot "0.12.2"]
                 [org.clojure/tools.logging "0.5.0"]
                 [cheshire "5.9.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [org.clojure/java.jdbc "0.7.10"]
                 [org.postgresql/postgresql "42.2.8"]]
  :main ^:skip-aot closure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
