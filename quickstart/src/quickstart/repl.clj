(ns quickstart.repl
  (:require [pallet.configure   :as configure]
            [pallet.compute     :as compute]
            [pallet.api         :as api]
            [pallet.actions     :as actions]
            [pallet.crate       :as crate]
            [pallet.node        :as node]
            [pallet.crate.automated-admin-user :refer [automated-admin-user]]))

(crate/defplan web-plan
  []
  (actions/package "nginx")
  (actions/remote-file "/usr/share/nginx/www/index.html"
                       :content "Welcome to Pallet!")
  (actions/service "nginx" :action :restart))

(def web-node
  (api/node-spec
   :image    {:image-id "7d36b6bc-ec34-4e76-beac-7d6e956b6527"}
   :hardware {:hardware-id "71004023-bb72-4a97-b1e9-bc66dfce9470"}))

(def web-group
  (api/group-spec :web
                  :phases {:configure web-plan
                           :bootstrap automated-admin-user}
                  :node-spec web-node))

(def service (delay (configure/compute-service "exoscale")))

(def ^:dynamic *service* nil)

(defn cluster
  ([n]
     (api/converge {web-group n} :compute (or *service* @service)))
  ([]
     (cluster 1)))

(defn providers
  []
  (compute/supported-providers))

(defn nodes
  []
  (compute/nodes @service))

(defn -main [& [arg]]
  (let [nodecount (if arg (Integer/parseInt arg) 1)]
    (printf "starting a cluster with %s nodes" nodecount)
    (cluster nodecount)
    (System/exit 0)))