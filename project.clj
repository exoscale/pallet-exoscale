(defproject ch.exoscale/pallet-exoscale "0.1.0"
  :description "A provider for using Pallet with exoscale, based on clostack."
  :url "https://github.com/exoscale/pallet-exoscale"
  :license {:name "All rights reserved"}
  :scm {:url "git@github.com:exoscale/pallet-exoscale.git"}
  :dependencies [[clostack               "0.1.3"]
                 [org.clojure/data.codec "0.1.0"]
                 [org.clojure/data.json  "0.2.2"]
                 [http.async.client      "0.5.2"]
                 [com.palletops/pallet   "0.8.0-beta.10"]])
