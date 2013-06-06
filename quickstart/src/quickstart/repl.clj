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
  (actions/package-manager :update)
  (actions/package "nginx")
  (actions/remote-file "/usr/share/nginx/www/index.html"
                       :content "Welcome to Pallet!")
  (actions/service "nginx" :action :restart))

(def web-node
  (api/node-spec
   :network  {:inbound-ports [22 80]}
   :image    {:os-family :ubuntu
              :os-version-matches "12.04"}
   :hardware {:min-cores 1
              :min-disk  50
              :min-ram   512}))

(def web-group
  (api/group-spec :web
                  :phases {:configure web-plan
                           :bootstrap automated-admin-user}
                  :node-spec web-node))

(def service (delay (configure/compute-service "exoscale")))

(defn cluster
  ([n]
     (api/converge {web-group n} :compute @service))
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