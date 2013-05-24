(ns pallet.compute.exoscale
  "exoscale provider for pallet"
  (:require
   [clojure.string :as string]
   [clojure.tools.logging :as logging :refer [debugf debug warnf]]
   [pallet.action-plan :as action-plan]
   [pallet.api :refer [make-user]]
   [pallet.compute :as compute]
   [pallet.compute.implementation :as implementation]
   [pallet.execute :as execute]
   [pallet.feature :refer [if-feature has-feature?]]
   [pallet.node :as node]
   [pallet.script :as script]
   [pallet.ssh.execute :refer [ssh-script-on-target]]
   [pallet.stevedore :as stevedore]
   [pallet.compute.cloudstack :as cs]
   pallet.environment))


;;; Meta
(defn supported-providers []
  ["exoscale"])

;;; ## Nodes

;;; ### Tags
(def pallet-group-tag "pallet-group")
(def pallet-image-tag "pallet-image")
(def pallet-state-tag "pallet-state")

(defn group-tag
  "Return the group tag for a group"
  [group-spec]
  {:key pallet-group-tag :value (name (:group-name group-spec))})

(defn image-tag
  "Return the image tag for a group"
  [group-spec]
  {:key pallet-image-tag
   :value (with-out-str
            (pr (-> group-spec
                    :image
                    (select-keys
                     [:image-id :os-family :os-version :os-64-bit]))))})

(defn state-tag
  "Return the state tag for a group"
  [node-state]
  {:key pallet-state-tag :value (with-out-str (pr node-state))})

(defn get-tag
  [info tag]
  (->> info
       (:tags)
       (filter (comp (partial = tag) :key))
       (first)
       (:value)))

(defn node-state-value
  "Return the value from the state-tag on a node's instance-info"
  [info]
  (when-let [state (get-tag info pallet-state-tag)]
    (read-string state)))

(defn node-image-value
  "Return the value from the image-tag on a node's instance-info"
  [info]
  (when-let [state (get-tag info pallet-image-tag)]
    (read-string state)))

(defn tag-instances [api ids & tags]
  (debugf "tag-instances %s %s" ids tags)
  (cs/request
   api
   :createTags
   {:resourcetype "userVm"
    :resourceids ids
    :tags tags}))

(defn tag-instances-for-group-spec
  [api group-spec instance-ids]
  (debugf "tag-instances-for-group-spec %s %s" group-spec instance-ids)
  (tag-instances
   api
   instance-ids
   (group-tag group-spec)
   (image-tag group-spec)))

(defn tag-instance-state
  "Update the instance's state"
  [api info state]
  (let [old-state (get-tag info pallet-state-tag)]
    (tag-instances
     api
     [(:instance-id info)]
     (state-tag (merge old-state state)))))

;;; ### Node
(deftype ExoscaleNode
    [service info]

  pallet.node/NodePackager
  (packager [this]
    (debugf "exoscale node info: %s" info)
    (case (keyword (pallet.node/os-family this))
      :ubuntu :aptitude
      :centos :rpm
      :aptitude))

  pallet.node/NodeImage
  (image-user [this]
    (make-user "root"))

  pallet.node/NodeProxy
  (proxy [this] nil)
  
  pallet.node/Node
  (ssh-port [node] 22)
  (primary-ip [node]
    (-> info
        :nic
        first
        :ipaddress))
  (private-ip [node] (:public-ip-address info))
  (is-64bit? [node] true)
  (group-name [node] (get-tag info pallet-group-tag))
  (os-family [node] (:os-family (node-image-value info)))
  (os-version [node] (:os-version (node-image-value info)))
  (hostname [node] (:displayname info))
  (id [node] (:id info))
  (running? [node] (= "Running" (:state info)))
  (terminated? [node] (= "Destroyed" (:state info)))
  (compute-service [node] service)
  Object
  (toString [this]
    (format "%s:%s (%s)"
            (:displayname info)
            (pallet.node/primary-ip this)
            (:state info))))

(defn bootstrapped?
  "Predicate for testing if a node is bootstrapped."
  [node]
  (:bs (node-state-value (.info node))))

;;; ### OS parsing
(def ^{:doc "Map of non-canonical os-versions to canonical versions"}
  os-versions
  {"centos" {"6" "6.0"}
   "ubuntu" {"precise" "12.04" "quantum" "12.10" "raring" "13.04"}})

(def ^{:doc "Map of non-canonical os-families to canonical versions"}
  os-families
  {})

(defn default-parser-result
  [match]
  (when-let [[_ family version] match]
    (let [family (string/lower-case family)
          versions (os-versions family {})]
      {:os-family (keyword (os-families family family))
       :os-version (versions version version)})))

(def os-parsers
  (array-map
   #"Ubuntu ([0-9]+\.[0-9]+)"
   (fn [match]
     (when match
       {:os-family :ubuntu
        :os-version (nth match 2)}))
   #"CentOS ([0-9]+\.[0-9]+)"
   (fn [match]
     (when match
       {:os-family :centos
        :os-version (nth match 2)}))))

(defn parse-image
  "Best guess os family and version from the image name"
  [description]
  (first
   (filter identity
           (map
            (fn [[re f]]
              (when-let [match (re-find re description)]
                (f match)))
            os-parsers))))

;;; implementation detail names
(defn security-group-name
  "Return the security group name for a group"
  [group-spec]
  (str "pallet-" (name (:group-name group-spec))))

(defn user-keypair-name
  [user]
  (debug "getting keypair name for: " user)
  (str "pallet-" (:username user)))


;;; Compute service
(defn ensure-keypair [api key-name user]
  (let [key-pairs (-> (cs/request api :listSSHKeyPairs
                                  {:name key-name})
                      :listsshkeypairsresponse
                      :keypair)]
    (debugf "ensure-keypair existing %s: %s" key-name key-pairs)
    (when (zero? (count key-pairs))
      (cs/request api :registerSSHKeyPair
                  {:name key-name
                   :publickey (slurp (:public-key-path user))}))))

(defn ensure-security-group [api security-group-name]
  (let [sgs (-> (cs/request api :listSecurityGroups
                            {:securitygroupname security-group-name})
                :listsecuritygroupsresponse
                :securitygroup)]
    (debugf "ensure-security-group existing %s" sgs)
    (when-not (seq sgs)
      (let [r  (cs/request api :createSecurityGroup
                           {:name security-group-name})
            id (-> r :createsecuritygroupresponse :securitygroup :id)]
        (when id
          (cs/request api :authorizeSecurityGroupIngress
                      {:cidrlist ["0.0.0.0/0"]
                       :securitygroupid id
                       :startport "22"
                       :endport "22"
                       :protocol "TCP"}))))))

(defn- get-tags [api node]
  (let [tags (cs/request
              api
              :listTags
              {:resourceid (node/id node)})]
    (debugf "get-tags tags %s" tags)
    (into {} (map (juxt :key :value) (:tags tags)))))

(deftype ExoscaleNodeTag [api]
  pallet.compute.NodeTagReader
  (node-tag [_ node tag-name]
    (debugf "node-tag %s %s" (node/id node) tag-name)
    (let [tags (get-tags api node)]
      (debugf "node-tag tags %s" tags)
      (get tags tag-name)))
  (node-tag [_ node tag-name default-value]
    (debugf "node-tag %s %s %s"
            (node/id node) tag-name default-value)
    (let [tags (get-tags api node)]
      (debugf "node-tag tags %s" tags)
      (get tags tag-name default-value)))
  (node-tags [_ node]
    (debugf "node-tags %s" (node/id node))
    (get-tags api node))

  pallet.compute.NodeTagWriter
  (tag-node! [_ node tag-name value]
    (debugf "tag-node! %s %s %s" (node/id node) tag-name value)
    (cs/request
     api
     :createTags
     {:resourcetype "userVm"
      :resourceids [(node/id node)]
      :tags [{:key tag-name :value value}]}))
  (node-taggable? [_ node]
    (debugf "node-taggable? %s" (node/id node))
    true))

(deftype ExoscaleService
    [api image-info environment instance-poller poller-future
     tag-provider]
  pallet.compute.ComputeService

  (nodes [service]
    (letfn [(make-node [info] (ExoscaleNode. service info))]
      (let [instances (-> (cs/request api :listVirtualMachines {}))]
        (debug "instances: " instances)
        (map make-node (-> instances :listvirtualmachinesresponse :virtualmachine)))))

  (ensure-os-family [_ {:keys [image group-name] :or {image {}} :as group-spec}]
    (when-not (:image-id image)
      (throw
       (ex-info
        (format "Group-spec %s :image does not specify an :image-id" group-name)
        {:group-spec group-spec
         :reason :no-image-id})))
    (if-let [missing-keys (seq (remove image [:os-family :os-version :user]))]
      (let [response (-> (cs/request
                          api
                          :listTemplates
                          {:templatefilter "featured"
                           :id (:image-id image)})
                         :listtemplatesresponse
                         :template)]
        (debugf "ensure-os-family images %s" response)
        (if-let [images (:images response)]
          (let [image (first images)]
            (let [image-details (parse-image (:name image))
                  group-spec (update-in
                              group-spec [:image]
                              (fn [image] (merge image-details image)))]
              (if (every? (:image group-spec) missing-keys)
                (do
                  (logging/warnf
                   (str
                    "group-spec %s :image does not specify the keys %s. "
                    "Inferred %s from the AMI %s with name \"%s\".")
                   group-name (vec missing-keys)
                   (zipmap missing-keys (map (:image group-spec) missing-keys))
                   (:image-id image) (:name image))
                  group-spec)
                (let [missing (vec (remove (:image group-spec) missing-keys))]
                  (throw
                   (ex-info
                    (format
                     (str "group-spec %s :image does not specify the keys %s. "
                          "Could not infer %s from the AMI %s with name %s.")
                     group-name (vec missing-keys) missing (:name image))
                    {:group-spec group-spec
                     :image-name (:name image)
                     :image-id (:image-id image)
                     :missing-keys missing}))))))
          (throw
           (ex-info
            (format "Image %s not found" (:image-id image))
            {:image-id (:image-id image)
             :reason :image-not-found}))))
      group-spec))

  (run-nodes [service group-spec node-count user init-script options]
    ;; need a keypair
    (debug "run-nodes with spec: " group-spec)
    (let [key-name (or (:keypair options)
                       (-> group-spec :image :config :key-name)
                       (let [key-name (user-keypair-name user)]
                         (ensure-keypair api key-name user)
                         key-name))
          security-group (-> group-spec :node-spec :config :security-group)
          _              (debug "got initial security-group: " security-group)
          security-group (if (seq security-group)
                           security-group
                           (let [security-group (security-group-name
                                                 group-spec)]
                             (ensure-security-group
                              api security-group)
                             security-group))
          zone      (or (-> group-spec :hardware :zone)
                        (-> (cs/request api :listZones {})
                            :listzonesresponse
                            :zone
                            first
                            :id))
          hardware-id (or (-> group-spec :hardware :hardware-id)
                          (-> (cs/request api :listServiceOfferings {:zoneid zone})
                              :listserviceofferingsresponse
                              :serviceoffering
                              first
                              :id))
          image-id  (-> group-spec :image :image-id)]
      (debugf "run-instances %s nodes" node-count)
      (when-let [jobs (cs/run-instances
                       api
                       :prefix (:group-name group-spec)
                       :count node-count
                       :keypair key-name
                       :image-id image-id
                       :hardware-id hardware-id
                       :zone zone
                       :security-groups [security-group])]
        (debugf "run-instances jobs: %s" (vec jobs))
        (doseq [job jobs]
          (cs/schedule instance-poller job))
        (letfn [(make-node [info] (ExoscaleNode. service info))]
          (let [instances (map (comp :virtualmachine :jobresult
                                     :queryasyncjobresultresponse)
                               (cs/waitall instance-poller jobs))
                ids       (map :id instances)]
            (debugf "run-instances instances: %s" instances)
            (debugf "run-instances tagging")
            (tag-instances-for-group-spec api group-spec ids)

            (let [running? (comp (partial = "Running") :state)]
              (when-let [failed (seq (filter (complement running?) instances))]
                (warnf "run-nodes Nodes failed to start %s" (vec failed)))
              (map make-node (filter running? instances))))))))

  (reboot [_ nodes])

  (boot-if-down [_ nodes])

  (shutdown-node [_ node user])

  (shutdown [self nodes user])

  (destroy-nodes-in-group [_ group-name]
    (let [nodes (cs/request
                 api
                 :listVirtualMachines
                 {:tags [{:key pallet-group-tag
                          :value group-name}]})]
      (doseq [{:keys [id]} (-> nodes :listvirtualmachinesresponse :virtualmachine)]
        (cs/request api :destroyVirtualMachine {:id id}))))

  (destroy-node [_ node]
    (cs/request api :destroyVirtualMachine {:id (node/id node)}))

  (images [_] @image-info)

  (close [_]
    (cs/deliverall instance-poller nil)
    (future-cancel poller-future))

  pallet.environment.Environment
  (environment [_] environment)

  pallet.compute.NodeTagReader
  (node-tag [compute node tag-name]
    (compute/node-tag
     (.tag_provider compute) node tag-name))
  (node-tag [compute node tag-name default-value]
    (compute/node-tag
     (.tag_provider compute) node tag-name default-value))
  (node-tags [compute node]
    (compute/node-tags (.tag_provider compute) node))
  pallet.compute/NodeTagWriter
  (tag-node! [compute node tag-name value]
    (compute/tag-node! (.tag_provider compute) node tag-name value))
  (node-taggable? [compute node]
    (when (.tag_provider compute)
      (compute/node-taggable? (.tag_provider compute) node))))


;; service factory implementation for exoscale
(defmethod implementation/service :exoscale
  [provider {:keys [api-key api-secret endpoint tag-provider environment] :as options}]
  (debug "using options: " options)
  (let [api (apply cs/http-client (apply concat (dissoc options :tag-provider)))
        tag-provider (or tag-provider (ExoscaleNodeTag. api))
        poller (cs/node-poller api 5000)]
    (ExoscaleService.
     api
     (atom {})
     environment
     poller
     (future (cs/poll poller))
     tag-provider)))
