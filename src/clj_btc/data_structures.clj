(ns clj-btc.data-structures
  (:require [clojure.core.typed :as T]))

;;; Types for core.typed
(T/def-alias ConfigMap (HMap :mandatory {:rpcpassword String, :rpcuser String,
                                         :rpchost String, :rpcport Integer,
                                         :testnet Boolean}))
(T/def-alias ResultMap (T/Map String (U String Boolean Number)))
(T/def-alias ErrorMap (T/Map String (U Integer String)))