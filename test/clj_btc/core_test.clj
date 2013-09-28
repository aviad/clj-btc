(ns clj-btc.core-test
  (:require [clojure.test :refer :all]
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

(deftest return-types
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

(deftest send-payment
  (let [imported-account
        (str "IMPORTED-" (subs (str (java.util.UUID/randomUUID)) 0 8))

        receiving-account (str "RECEIVING-"
                               (subs (str (java.util.UUID/randomUUID))
                                     0 8))
        receiving-address (getaccountaddress :config @cfg
                                             :account receiving-account)]
    (prn "importing private key and rescaning, this may take a few seconds.")
    (when (nil?
           (importprivkey :config @cfg
                          :bitcoinprivkey (:private addr-with-bitcoins)
                          :label imported-account :rescan true))
      ;; import was successful
      (sendtoaddress :bitcoinaddress receiving-address :amount 1E-8
                     :comment "sending 1 (testcoin) satoshi.to test btc-clj.")
      (is (= 1E-8 (getbalance :config @cfg :account receiving-account
                              :minconf 0))))))
