;;;; Copyright 2013 Aviad Reich.
;;;; The use and distribution terms for this software are covered by
;;;; the Apache License, Version 2.0
;;;; (http://www.apache.org/licenses/LICENSE-2.0.txt), which can be
;;;; found in the file LICENSE at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound
;;;; by the terms of this license. You must not remove this notice, or
;;;; any other, from this software.

(ns clj-btc.config
  (:require [clojure.java.io :refer (reader file)]
            [clojure.string :refer (split)]
            [clojure.core.typed :as T]
            [clj-btc.data-structures :refer (ConfigMap)]))

(set! *warn-on-reflection* true)

;;; Types for required functions:
(T/ann ^:no-check clojure.java.io/reader [String -> java.io.Reader])
(T/ann ^:no-check clojure.java.io/file [String * -> java.io.File])
(T/ann ^:no-check clojure.string/split
       [String java.util.regex.Pattern -> (T/NonEmptyVec String)])
;;; FIXME: The next annotation is not working for some reason..
(T/ann ^:no-check System/getProperty (Fn [String -> (T/Option String)]
                                         [String String -> String]))

;;; config file related functions
(T/ann default-config-file [ -> String])
(defn- default-config-file
  "Return the full path (as a vector of strings) to the default bitcoin.conf
   file, by OS (default Linux). This is in accordance with
   https://en.bitcoin.it/wiki/Running_Bitcoin#Bitcoin.conf_Configuration_File"
  []
  (let [nix-data-dir #(or (System/getProperty "user.home") "")
        win-data-dir #(or (System/getenv "AppData") "")
        os-name (or (System/getProperty "os.name") "")
        path (case (first (split os-name #"\s"))
               "Mac" [(nix-data-dir) "Library" "Application Support"
                      "Bitcoin" "bitcoin.conf"]
               "Windows" [(win-data-dir) "Bitcoin" "bitcoin.conf"]
               ;; "Linux" is the default
               [(nix-data-dir) ".bitcoin" "bitcoin.conf"])]
    (str (apply file path))))

(T/ann parse-config [String -> ConfigMap])
;; Straight from http://stackoverflow.com/questions/7777882/loading-configuration-file-in-clojure-as-data-structure
(defn parse-config [file-name]
  "read the file according to the given "
  (let [config
        (with-open [reader (reader file-name)]
          (let [props (java.util.Properties.)]
            (.load props reader)
            (T/ann-form
             (into {} (for [[k v] (T/ann-form props java.util.Properties)]
                        [(keyword (T/ann-form k String)) (read-string v)])))
            ConfigMap))
        testnet (and (integer? (:testnet config))
                     (> (:testnet config) 0))]
    ;; add default values
    (merge {:rpcport (if testnet 18332 8332),
            :rpchost "http://127.0.0.1"}
           (assoc config :testnet testnet))))

(T/ann read-local-config
       (Fn [ -> ConfigMap]
           [String -> ConfigMap]))
(defn read-local-config
  "Return a Map of properties from the given file, or from the default
   configuration file"
  ([] (read-local-config (default-config-file)))
  ([file-name] (parse-config file-name)))
