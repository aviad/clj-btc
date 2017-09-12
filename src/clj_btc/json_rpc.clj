(ns clj-btc.json-rpc
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.client :as http])
  (:require [clojure.java.io :as jio :refer (reader)])
  (:require [clj-btc.config :refer (read-local-config)])
  (:require [taoensso.timbre :as log])
  (:import java.io.StringReader))

(declare rpc-backend-call retry)

(def ^java.util.concurrent.atomic.AtomicInteger id-num
  (java.util.concurrent.atomic.AtomicInteger.))

(defn rpc-call
  "Perform rpc call of method with params, according to config.
   Might throw an exception in case of a non-bitcoin error
   (i.e a connection is refused)."
  [config method params]
  (let [hosts (:rpchost config)
        hosts (if (vector? hosts) hosts [hosts])
        ports (:rpcport config)
        ports (if (vector? ports) ports [ports])
        connections (map (fn [h p] {:rpchost h :rpcport p}) hosts ports)
        connections (shuffle connections)
        do-rpc (fn [conn]
                   (rpc-backend-call (into config conn) method params))
        conf (into config {
          :rpchost (if (vector? hosts) hosts [hosts])
          :rpcport (if (vector? ports) ports [ports])})]
    (retry do-rpc connections)))

(defn- retry [f args]
  (let [[x & xs] args
        resp (f x)]
    (if-let [err (:error resp)]
      (if (empty? xs)
        (throw err)
        (recur f xs))
      resp)))

(defn- rpc-backend-call
  "Perform rpc call of method with params, according to config.
   Might throw an exception in case of a non-bitcoin error
   (i.e a connection is refused)."
  [config method params]
  (let [logger (:logger config)
        log (fn [msg] (when (fn? logger) (logger msg)))
        host (:rpchost (log/spy config))
        resp
        @(http/post
          (str (config :rpchost) ":" (config :rpcport))
          {:basic-auth [(config :rpcuser) (config :rpcpassword)],
           :headers {"Content-Type" "application/json; charset=utf-8"},
           :body (json/write-str {"version" "1.1", "params" params,
                                  "method" method,
                                  "id" (.incrementAndGet id-num)})})
        err (:error resp)
        payload (and (not err)
                     (-> resp :body StringReader. (json/read :bigdec true)))]
    (cond
      err (do (log (str "RPC call to " method " on " host " failed: " err))
              resp)
      (= 200 (:status resp)) (get payload "result")
      :else (do (logger (str "RPC call to " method " on " host " failed: "
                             (get payload "error")))
                (get payload "error")))))
