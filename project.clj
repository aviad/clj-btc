(defproject clj-btc "0.2.0-SNAPSHOT"
  :description "Clojure bindings for the original (C++) Bitcoin Client"
  :url "https://github.com/aviad/clj-btc"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.3"]
                 [http-kit "2.1.11"]
                 [clj-http "0.7.7"]
                 [org.clojure/core.typed "0.2.13"]]
  :core.typed {:check [clj-btc.core clj-btc.config clj-btc.data-structures]})
