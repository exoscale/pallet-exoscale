(ns pallet.compute.cloudstack
  "A mostly generated wrapper to the cloudstack API."
  (:require [clojure.string            :as str]
            [clojure.data.codec.base64 :as base64]
            [clojure.data.json         :as json]
            [http.async.client         :as http]
            [clojure.tools.logging     :refer [debugf info warn]])
  (:import java.net.URLEncoder
           javax.crypto.spec.SecretKeySpec
           javax.crypto.Mac
           java.security.MessageDigest
           java.security.SecureRandom))

(defn url-encode
  "Encode URL"
  [s]
  (URLEncoder/encode s "UTF-8"))

(defn quote-plus
  "Replace + in encoded URL by %20"
  [s]
  (str/replace (str s) "+" "%20"))

(defprotocol CloudstackClient
  (get-url [this opcode args] "Yield the URL to query for an opcode and args")
  (async-request [this opcode args] "Send out HTTP query, return deferred")
  (request [this opcode args] "Send out request, return output body"))

(defrecord ArgPair [k v]
  clojure.lang.IFn
  (toString [this]
    (str (str/lower-case (name k)) "=" (url-encode v)))
  java.lang.Comparable
  (compareTo [this other]
    (compare k (:k other))))

(defn get-signature
  "Build a SHA1 Digest of all args to send off to the API"
  [secret args]
  (let [key         (SecretKeySpec. (.getBytes secret) "HmacSHA1")
        mac         (doto (Mac/getInstance "HmacSHA1") (.init key))
        input       (str/join "&" (map (comp quote-plus str/lower-case) args))
        into-string #(String. %)]
    (-> (.doFinal mac (.getBytes input))
        (base64/encode)
        (into-string)
        (str/trim)
        (url-encode))))

(defn add-signature
  "Add computed signature to the list of args"
  [secret args]
  (let [signature (get-signature secret args)
        args      (conj (vec args) (format "signature=%s" signature))]
    (map quote-plus args)))

(defn parse-response
  "Given a complete HTTP object, deserialize"
  [resp]
  (let [s (-> resp (http/string))]
    (debugf "received response: %s" s)
    (try
      (json/read-str s :key-fn keyword)
      (catch Exception e
        {:status (http/status resp) :unknown s}))))

(defrecord CloudstackHTTPClient [api-key api-secret endpoint http-client]
  CloudstackClient
  (async-request [this opcode args]
    (debugf "carrying out: %s with args: %s" opcode args)
    (->> (for [[k v] (assoc args
                       :command opcode
                       :apikey api-key
                       :response :json)
               :let [k (str/lower-case (name k))
                     v (if (keyword? v) (name v) v)]
               :when (and v (if (sequential? v) (not-empty v) true))]
           (cond (and (sequential? v) (-> v first map?))
                 (for [[i submap] (map-indexed vector v)]
                   (for [[subk subv] submap
                         :when (seq subv)
                         :let [subk (str/lower-case (name subk))
                               subv (name subv)]]
                     (ArgPair. (format "%s[%d].%s" k i subk subv) subv)))
                 (sequential? v)
                 (ArgPair. (name k) (str/join "," (map name (remove nil? v))))
                 :else
                 (ArgPair. (name k) v)))
         (flatten)
         (sort)
         (map str)
         (add-signature api-secret)
         (str/join "&")
         (str endpoint "?")
         (http/GET http-client)))
  (request [this opcode args]
    (-> (async-request this opcode args)
        (http/await)
        (parse-response))))

(defn get-envopt
  "Fetch option either from system properties or environment variables"
  [n]
  (or (System/getProperty n)
      (System/getenv (-> n
                         (str/replace "." "_")
                         str/upper-case))))

(defn run-instances
  [api & {:keys [prefix count keypair security-groups
                 image-id hardware-id zone]}]
  (let [rnd    (SecureRandom.)
        getrnd #(-> rnd (.nextInt 4095) Integer/toHexString)]
    (for [suffix (repeatedly count getrnd)
          :let [fullname (str (name prefix) "-" suffix)]]
      (-> (request api :deployVirtualMachine
                   {:zoneid            zone
                    :templateid        image-id
                    :serviceofferingid hardware-id
                    :keypair           keypair
                    :name              fullname
                    :securitygroupnames  security-groups})
          :deployvirtualmachineresponse
          :jobid))))

(defn http-client
  "Create an HTTP client"
  [& {:keys [api-key api-secret endpoint http]
      :or   {api-key    (get-envopt "cloudstack.api.key")
             api-secret (get-envopt "cloudstack.api.secret")
             endpoint   (get-envopt "cloudstack.endpoint")
             http       (http/create-client)}}]
  (CloudstackHTTPClient. api-key api-secret endpoint http))

(defprotocol NodePoller
  (poll [this])
  (deliverall [this v])
  (schedule [this jobid])
  (waitall [this jobids])
  (waitfor [this jobid]))

(defn node-poller
  [api interval]
  (let [jobmap (atom {})]
    (reify NodePoller
      (poll [this]
        (loop []
          (doseq [[jobid jobpromise]  @jobmap
                  :let [res (request api :queryAsyncJobResult {:jobid jobid})]]
            (when (= 1 (-> res :queryasyncjobresultresponse :jobstatus))
              (swap! jobmap dissoc jobid)
              (deliver jobpromise res)))
          (Thread/sleep interval)
          (recur)))
      (deliverall [this v]
        (doseq [[jobid jobpromise] @jobmap]
          (deliver jobpromise nil)))
      (waitall [this jobids]
        (-> (map (partial waitfor this) jobids)
            (doall)
            (vec)))
      (schedule [this jobid]
        (when jobid
          (swap! jobmap assoc jobid (promise))))
      (waitfor [this jobid]
        (when-let [jobpromise (get @jobmap jobid)]
          (deref jobpromise))))))
