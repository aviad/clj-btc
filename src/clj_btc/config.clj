(ns clj-btc.config
  (:require [clojure.java.io :as jio])
  (:require [clojure.string :refer (split)]))

;;; config file related functions
(defn- default-config-file
  "Return the full path (as a vector of strings) to the default bitcoin.conf
   file, by OS (default Linux). This is in accordance with
   https://en.bitcoin.it/wiki/Running_Bitcoin#Bitcoin.conf_Configuration_File"
  []
  (let [nix-data-dir #(System/getProperty "user.home")
        win-data-dir #(System/getenv "AppData")
        os-name (System/getProperty "os.name")
        path (case (first (split os-name #"\s"))
               "Mac" [(nix-data-dir) "Library" "Application Support"
                      "Bitcoin" "bitcoin.conf"]
               "Windows" [(win-data-dir) "Bitcoin" "bitcoin.conf"]
               ;; "Linux" is the default
               [(nix-data-dir) ".bitcoin" "bitcoin.conf"])]
    (str (apply jio/file path))))

(defn parse-config
  "Return a Map of properties from the given file, or from the default
   configuration file"
  ([] (parse-config (default-config-file)))
  ;; Straight from http://stackoverflow.com/questions/7777882/loading-configuration-file-in-clojure-as-data-structure
  ([file-name]
     (let [config
           (with-open [^java.io.Reader reader (jio/reader file-name)]
             (let [props (java.util.Properties.)]
               (.load props reader)
               (into {} (for [[k v] props] [(keyword k) (read-string v)]))))
           ;; add default values
           testnet (and (number? (config :testnet)) (> (config :testnet) 0))
           rpcport (get config :rpcport (if testnet 18332 8332))
           rpchost (get config :rpchost "http://127.0.0.1")]
       (assoc config :testnet testnet :rpcport rpcport :rpchost rpchost))))
