;;;; Copyright 2013 Aviad Reich.
;;;; The use and distribution terms for this software are covered by
;;;; the Apache License, Version 2.0
;;;; (http://www.apache.org/licenses/LICENSE-2.0.txt), which can be
;;;; found in the file LICENSE at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound
;;;; by the terms of this license. You must not remove this notice, or
;;;; any other, from this software.

(ns clj-btc.core-test
  (:require [clojure.set :refer (superset?)]
            [clojure.test :refer :all]
            [clj-btc.core :refer :all]
            [clj-btc.config :refer :all]))

;;; Since clj-btc is a wrapper around the C++ Bitcoin client, all the
;;; tests are integration tests - making sure the different functions
;;; are able to return the correct values, assuming that a
;;; "Bitcoin-Qt -server"/"bitcoind" is running and accessible according to the
;;; local configuration file.

(def cfg (atom {}))

(def addr-with-bitcoins
  {:public "mqWiQRdS6MePCtALRg2smULFvqf8Ru1usj",
   :private "cVGghpMADPnrX6YNnS7X8nSEnULR6epfJFHLyw9dvKi6n17tUkDg"})

(defn config-fixture
  [f]
  (let [conf (read-local-config)
        info (getinfo :confing @cfg)]
    ;;; only test on testnet
    (when (info "testnet")
      (reset! cfg conf))
    (f)))

(use-fixtures :once config-fixture)

(defmacro is-type
  "Test an expression is of a given type and issues a corresponding
  message if it isn't. The expected-type can be either a function
  (i.e. integer?) or a class (i.e. BigDecimal)."
  [expected-type rpc-method & args]
  (let [type-test (if (class? (resolve expected-type))
                    `(instance? ~expected-type)
                    (list expected-type))]
    `(is (~@type-test ~(concat (list rpc-method :config '@cfg) args))
         (str ~rpc-method " should return a result of type "
              ~(str expected-type)))))

(defn- supermap?
  "Does m1 cointain all of m2? (inspired by https://stackoverflow.com/questions/20421405/how-to-check-if-a-map-is-a-subset-of-another-in-clojure)"
  [m1 m2]
  (superset? (set m1) (set m2)))

(deftest return-types
  (is-type BigDecimal estimatefee :blocks 6)
  (is-type integer? getblockcount)
  (is-type integer? getconnectioncount)
  (is-type BigDecimal getdifficulty)
  (is-type integer? gethashespersec)
  (is-type integer? getblockcount)
  (is-type vector? listreceivedbyaddress)
  (is-type vector? listreceivedbyaccount)
  (is-type vector? listunspent))

(deftest generate
  (is-type Boolean getgenerate)
  ;; setting generate to True
  (setgenerate :confing @cfg :generate true)
  (is
   (getgenerate :confing @cfg)
   "getgenerate should be true after the use of (setgenerate :generate true)")
  ;; set generate to true and max processors to 2
  (setgenerate :confing @cfg :generate true :genproclimit 2)
  (is (getgenerate :confing @cfg)
      (str "getgenerate should be true after the use of "
           "(setgenerate :generate true :genproclimit 2)"))
  (setgenerate :confing @cfg :generate false))

(deftest accounts-creation
  (let [account (str (java.util.UUID/randomUUID))
        bitcoinaddress (getnewaddress :config @cfg :account account)
        ;; for side-effects.
        _ (setaccount :config @cfg :bitcoinaddress bitcoinaddress
                      :account account)
        bitcoinaddress (getaccountaddress :config @cfg :account account)
        addresses (getaddressesbyaccount :config @cfg :account account)]
    (is (string? bitcoinaddress) "getnewaddress should return a string")
    (is (= (getaccount :config @cfg :bitcoinaddress bitcoinaddress) account))
    (is (some #{bitcoinaddress} addresses))
    (is (zero? (getreceivedbyaddress :config @cfg
                                   :bitcoinaddress bitcoinaddress))
        "newly created bitcoinaddress' balance is 0")
    (is (zero? (getreceivedbyaccount :config @cfg :account account)))
    (is ((validateaddress :config @cfg :bitcoinaddress bitcoinaddress)
         "isvalid"))
    (is (false? ((validateaddress :config @cfg
                                  :bitcoinaddress "invalid-address")
         "isvalid")))))

(deftest raw-transactions
  (let [empty-raw-tx "01000000000000000000"]
    (is (= empty-raw-tx (createrawtransaction :txids-map []
                                              :addrs-amounts-map {}))
        "empty raw transaction creation correct")

    (is (= (createrawtransaction
            :txids-map
            [{"txid" "7acb74b4ad7f982eed3dabf93c8f474451b0a60f3f7950cd12a2bbe721290cb8"
              "vout" 0}]
            :addrs-amounts-map {"mqWiQRdS6MePCtALRg2smULFvqf8Ru1usj" 9.87654321})
           "0100000001b80c2921e7bba212cd50793f0fa6b05144478f3cf9ab3ded2e987fadb474cb7a0000000000ffffffff01b168de3a000000001976a9146da5a9b587c385c1a6002bf77f71e4333e0ceb1a88ac00000000")
        "raw transaction creation correct")

    (let [random-vout (rand-int 10000)
          random-amount (bigdec (format "%.8f" (rand)))
          raw-transaction (createrawtransaction
                           :txids-map
                           [{"txid" "7acb74b4ad7f982eed3dabf93c8f474451b0a60f3f7950cd12a2bbe721290cb8"
                             "vout" random-vout}]
                           :addrs-amounts-map
                           {"mqWiQRdS6MePCtALRg2smULFvqf8Ru1usj" random-amount})
          decoded-raw-transaction (decoderawtransaction :hex-string raw-transaction)]
      (is (supermap?
           decoded-raw-transaction
           {;; "txid" "6316b2abcc2d957ecb1304f63725ea70841b1feae94c6f017e114556d8c60537",
            "version" 1, "locktime" 0,
            "vin" [{"txid" "7acb74b4ad7f982eed3dabf93c8f474451b0a60f3f7950cd12a2bbe721290cb8",
                    "vout" random-vout, "scriptSig" {"asm" "", "hex" ""},
                    "sequence" 4294967295}],
            "vout" [{"value" random-amount, "n" 0,
                     "scriptPubKey"
                     {"asm" "OP_DUP OP_HASH160 6da5a9b587c385c1a6002bf77f71e4333e0ceb1a OP_EQUALVERIFY OP_CHECKSIG",
                      "hex" "76a9146da5a9b587c385c1a6002bf77f71e4333e0ceb1a88ac",
                      "reqSigs" 1, "type" "pubkeyhash", "addresses" ["mqWiQRdS6MePCtALRg2smULFvqf8Ru1usj"]}}]})
          "decoding encoded raw transaction correct"))
    (is (= (getrawtransaction :txid "7acb74b4ad7f982eed3dabf93c8f474451b0a60f3f7950cd12a2bbe721290cb8")
           "01000000019c19404b3cd4aa272cdc45fd6362f33151181d6ea6f26bda8d6311664e8a2248000000006b483045022065939207fde542fd6f38da9c651537f3e36621e9a447f2877ff0e8a807137367022100c7935881cd3af35502eb12a7ea12d7a2952ac5e1ada1dd52bef1b9449fca36240121024d1a5f75a170eebebe25df71a87e3ef012cc07754db4c87feeee65f1aeed84bfffffffff0210270000000000001976a914fac7cf4845a26094ff9571a27db6268950a3b70e88ac3214ac3a000000001976a914560b13a3ac034489c8bc9488d749174a2fb198e988ac00000000")
        "get raw transaction")

    (is (= (signrawtransaction
            :hexstring "010000000189957b01aed596d3b361b576234eaeed3249246f14562d6bc6085166cd247d5a0000000000ffffffff0180969800000000001976a9140dfc8bafc8419853b34d5e072ad37d1a5159f58488ac00000000"
            :txinfo [{:txid "7acb74b4ad7f982eed3dabf93c8f474451b0a60f3f7950cd12a2bbe721290cb8" :vout 2 :scriptPubKey "123d"}])
           {"hex" "010000000189957b01aed596d3b361b576234eaeed3249246f14562d6bc6085166cd247d5a0000000000ffffffff0180969800000000001976a9140dfc8bafc8419853b34d5e072ad37d1a5159f58488ac00000000",
            "complete" false})
        "raw transaction not signed")))




;; (deftest send-payment
;;   (let [imported-account
;;         (str "IMPORTED-" (subs (str (java.util.UUID/randomUUID)) 0 8))

;;         receiving-account (str "RECEIVING-"
;;                                (subs (str (java.util.UUID/randomUUID))
;;                                      0 8))
;;         receiving-address (getaccountaddress :config @cfg
;;                                              :account receiving-account)]
;;     (prn "importing private key and rescaning, this may take a few seconds.")
;;     (when (nil?
;;            (importprivkey :config @cfg
;;                           :bitcoinprivkey (:private addr-with-bitcoins)
;;                           :label imported-account :rescan true))
;;       ;; import was successful
;;       (sendtoaddress
;;        :config @cfg :bitcoinaddress receiving-address :amount 1E-4
;;        :comment "sending 1E-4 (testcoin) BTC .to test clj-btc.")
;;       (is (= 1E-4M (getbalance :config @cfg :account receiving-account
;;                                :minconf 0))))
;;     (prn "done.")))
