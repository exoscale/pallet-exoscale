(defproject quickstart "0.1.0"
  :description "quickstart for pallet-exoscale"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.palletops/pallet "0.8.0-beta.10"]
                 [ch.qos.logback/logback-classic "1.0.0"]
                 [ch.exoscale/pallet-exoscale "0.1.2"]]
  :main quickstart.repl
  :profiles {:dev
             {:dependencies
              [[com.palletops/pallet "0.8.0-beta.10" :classifier "tests"]]}}
  :local-repo-classpath true)
