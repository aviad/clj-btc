;;;; Copyright 2013-2016 Aviad Reich.
;;;; The use and distribution terms for this software are covered by
;;;; the Apache License, Version 2.0
;;;; (http://www.apache.org/licenses/LICENSE-2.0.txt), which can be
;;;; found in the file LICENSE at the root of this distribution. By
;;;; using this software in any fashion, you are agreeing to be bound
;;;; by the terms of this license. You must not remove this notice, or
;;;; any other, from this software.

(ns clj-btc.core
  (:require [clojure.data.json :as json])
  (:require [clj-btc.json-rpc :as rpc])
  (:require [clojure.java.io :as jio :refer (reader)])
  (:require [clj-btc.config :refer (read-local-config)])
  (:import java.io.StringReader))

(set! *warn-on-reflection* true)

(def ^java.util.concurrent.atomic.AtomicInteger id-num
  (java.util.concurrent.atomic.AtomicInteger.))

(def not-nil? (comp not nil?))

(defn- do-rpc
  [name doc args premap]
  (let [args-form ['& {:keys (vec (cons 'config args))}]]
    `(defn ~name ~doc ~args-form
       ~premap
       (let [~'config (or ~'config (read-local-config))
             params# (vec (take-while not-nil? ~args))]
         (assert (map? ~'config))
         (rpc/rpc-call ~'config ~(str name) params#)))))

(defmacro ^:private defrpc
  "Create a method for rpc. Optional parameters end with a '?'.
   Defining pretests is the way to handle required parameters."
  [name doc args & premap]
  (do-rpc name doc args (first premap)))

;;; Remote Procedure Calls
(defrpc addmultisigaddress
  "Add a nrequired-to-sign multisignature address to the wallet. Each key is
   a bitcoin address or hex-encoded public key. If [account] is specified,
   assign address to [account]."
  [nrequired keys account]
  {:pre [(integer? nrequired)
         (vector? keys)]})

;;; ToDo: change add-remove-onetry to keyword-based
(defrpc addnode
  "(version 0.8) Attempts add or remove <node> from the addnode list or try a
   connection to <node> once."
  [node add-remove-onetry]
  {:pre [(string? node)
         (string? add-remove-onetry)]})

(defrpc backupwallet
  "Safely copies wallet.dat to destination, which can be a directory or a path
   with filename."
  [destination]
  {:pre [(string? destination)]})

(defrpc createmultisig
  "Creates a multi-signature address and returns a json object"
  [nrequired keys]
  {:pre [(integer? nrequired)
         (vector? keys)]})

(defrpc createrawtransaction
  "(version 0.7) Creates a raw transaction spending given inputs:
   [{\"txid\": txid \"vout\": n}...] {address:amount...}."
  [txids-map addrs-amounts-map]
  {:pre [(vector? txids-map)
         (map? addrs-amounts-map)]})

(defrpc decoderawtransaction
  "(version 0.7) Produces a human-readable JSON object for a raw transaction."
  [hex-string]
  {:pre (string? hex-string)})

(defrpc dumpprivkey
  "Reveals the private key corresponding to <bitcoinaddress>"
  [bitcoinaddress]
  {:pre [(string? bitcoinaddress)]})

(defrpc encryptwallet
  "Encrypts the wallet with <passphrase>."
  [passphrase]
  {:pre [(string? passphrase)]})

(defrpc estimatefee
  "<blocks> is an int specifying how many blocks you're willing
  to wait before confirmation"
  [blocks]
  {:pre [(integer? blocks)]})

(defrpc getaccount
  "Returns the account associated with the given address."
  [bitcoinaddress]
  {:pre [(string? bitcoinaddress)]})

(defrpc getaccountaddress
  "Returns the current bitcoin address for receiving payments to this account."
  [account]
  {:pre [(string? account)]})

(defrpc getaddednodeinfo
  "(version 0.8) Returns information about the given added node, or all added
   nodes.(note that onetry addnodes are not listed here) If dns is false, only
   a list of added nodes will be provided, otherwise connected information will
   also be available."
  [dns node]
  {:pre [(string? dns)]})

(defrpc getaddressesbyaccount
  "Returns the list of addresses for the given account."
  [account]
  {:pre [(string? account)]})

(defrpc getbalance
  "If [account] is not specified, returns the server's total available balance.
   If [account] is specified, returns the balance in the account."
  [account minconf])

(defrpc getblock
  "Returns information about the block with the given hash."
  [hash]
  {:pre [(string? hash)]})

(defrpc getblockcount
  "Returns the number of blocks in the longest block chain."
  [])

(defrpc getblockhash
  "Returns hash of block in best-block-chain at <index>; index 0 is the genesis
   block."
  [index]
  {:pre [(integer? index)]})

;; (defrpc getblocknumber
;;   "Deprecated. Removed in version 0.7. Use getblockcount."
;;   [])

(defrpc getblocktemplate
  "Returns data needed to construct a block to work on"
  [params])

(defrpc getconnectioncount
  "Returns the number of connections to other nodes."
  [])

(defrpc getdifficulty
  "Returns the proof-of-work difficulty as a multiple of the minimum
   difficulty."
  [])

(defrpc getgenerate
  "Returns true or false whether bitcoind is currently generating hashes"
  [])

(defrpc getinfo
  "Returns an object containing various state info."
  [])

(defrpc getmemorypool
  "Replaced in v0.7.0 with getblocktemplate, submitblock, getrawmempool```"
  [data])

(defrpc getmininginfo
  "Returns an object containing mining-related information: blocks,
   currentblocksize, currentblocktx, difficulty, errors, generate, genproclimit,
   hashespersec, pooledtx, testnet"
  [])

(defrpc getnewaddress
  "Returns a new bitcoin address for receiving payments. If [account] is
   specified (recommended), it is added to the address book so payments received
   with the address will be credited to [account]."
  [account])

(defrpc getpeerinfo
  "(version 0.7) Returns data about each connected node."
  [])

(defrpc getrawmempool
  "(version 0.7) Returns all transaction ids in memory pool"
  [])

(defrpc getrawtransaction
  "(version 0.7) Returns raw transaction representation for given
  transaction id."
  [txid verbose]
  {:pre [(string? txid)]})

(defrpc getreceivedbyaccount
  "Returns the total amount received by addresses with [account] in transactions
   with at least [minconf] confirmations. If [account] not provided return will
   include all transactions to all accounts. (version 0.3.24)"
  [account minconf])

(defrpc getreceivedbyaddress
  "Returns the total amount received by <bitcoinaddress> in transactions with at
   least [minconf] confirmations. While some might consider this obvious, value
   reported by this only considers *receiving* transactions. It does not check
   payments that have been made *from* this address. In other words, this is not
   \"getaddressbalance\". Works only for addresses in the local wallet, external
   addresses will always show 0."
  [bitcoinaddress minconf]
  {:pre [(string? bitcoinaddress)]})

(defrpc gettransaction
  "Returns an object about the given transaction containing:
   \"amount\": total amount of the transaction,
   \"confirmations\": number of confirmations of the transaction,
   \"txid\": the transaction ID,
   \"time\": time associated with the transaction.,
   \"details\" - An array of objects containing: \"account\", \"address\",
        \"category\", \"amount\", \"fee\""
  [txid]
  {:pre [(string? txid)]})

(defrpc gettxout
  "Returns details about an unspent transaction output (UTXO)"
  [txid n includemempool]
  {:pre [(string? txid)
         (integer? n)]})

(defrpc gettxoutsetinfo
  "Returns statistics about the unspent transaction output (UTXO) set"
  [])

(defrpc help
  "List commands, or get help for a command."
  [command])

(defrpc importprivkey
  "Adds a private key (as returned by dumpprivkey) to your wallet. This
   may take a while, as a rescan is done, looking for existing
  transactions. Optional [rescan] parameter added in 0.8.0."
  [bitcoinprivkey label rescan]
  {:pre [(string? bitcoinprivkey)]})

(defrpc keypoolrefill
  "Fills the keypool, requires wallet passphrase to be set."
  [])

(defrpc listaccounts
  "Returns Object that has account names as keys, account balances as
   values."
  [minconf])

(defrpc listaddressgroupings
  "(version 0.7) Returns all addresses in the wallet and info used for
   coincontrol."
  [])

(defrpc listreceivedbyaccount
  "Returns an array of objects containing:, \"account\": the account
   of the receiving addresses, \"amount\": total amount received by
   addresses with this account, \"confirmations\": number of
   confirmations of the most recent transaction included"
  [minconf includeempty])

(defrpc listreceivedbyaddress
  "Returns an array of objects containing:, \"address\": receiving
  address, \"account\": the account of the receiving address,
  \"amount\": total amount received by the address, \"confirmations\":
  number of confirmations of the most recent transaction included, To
  get a list of accounts on the system, execute bitcoind
  listreceivedbyaddress 0 true"
  [minconf includeempty]
  )
(defrpc listsinceblock
  "Get all transactions in blocks since block [blockhash], or all
   transactions if omitted."
  [blockhash target-confirmations])

(defrpc listtransactions
  "Returns up to [count] most recent transactions skipping the first
  [from] transactions for account [account]. If [account] not provided
  will return recent transaction from all accounts."
  [account count from])

(defrpc listunspent
  "(version 0.7) Returns array of unspent transaction inputs in the wallet."
  [minconf maxconf])
(defrpc listlockunspent
  "(version 0.8) Returns list of temporarily unspendable outputs"
  [])

(defrpc lockunspent
  "(version 0.8) Updates list of temporarily unspendable outputs"
  [unlock? array-of-objects]
  {:pre [(not-nil? unlock?)]})

(defrpc move
  "Move from one account in your wallet to another"
  [fromaccount toaccount amount minconf comment]
  {:pre [(string? fromaccount)
         (string? toaccount)
         (number? amount)]})

(defrpc sendfrom
  "<amount> is a real and is rounded to 8 decimal places. Will send
  the given amount to the given address, ensuring the account has a
  valid balance using [minconf] confirmations. Returns the transaction
  ID if successful (not in JSON object)."
  [fromaccount tobitcoinaddress amount minconf comment comment-to]
  {:pre [(string? fromaccount)
         (string? tobitcoinaddress)
         (number? amount)]})

(defrpc sendmany
  "amounts are double-precision floating point numbers.
  example usage:
  (sendmany :fromaccount \"my-account\"
            :address-amount-maps {\"payee1\" \"amount1\" ...}
            :minconf 6 :comment \"Keep the change\")"
  [fromaccount address-amount-maps minconf comment]
  {:pre [(string? fromaccount)
         (map? address-amount-maps)]})

(defrpc sendrawtransaction
  "(version 0.7) Submits raw transaction (serialized, hex-encoded) to
  local node and network."
  [hexstring]
  {:pre [(string? hexstring)]})

(defrpc sendtoaddress
  "<amount> is a real and is rounded to 8 decimal places. Returns the
  transaction ID <txid> if successful."
  [bitcoinaddress amount comment comment-to]
  {:pre [(string? bitcoinaddress)
         (number? amount)]})

(defrpc setaccount
  "Sets the account associated with the given address. Assigning
  address that is already assigned to the same account will create a new
  address associated with that account."
  [bitcoinaddress account]
  {:pre [(string? bitcoinaddress)
         (string? account)]})

(defrpc setgenerate
  "<generate> is true or false to turn generation on or
  off. Generation is limited to [genproclimit] processors, -1 is
  unlimited."
  [generate genproclimit]
  {:pre [(instance? Boolean generate)]})

(defrpc settxfee
  "<amount> is a real and is rounded to the nearest 0.00000001"
  [amount]
  {:pre [(number? amount)]})

(defrpc signmessage
  "Sign a message with the private key of an address."
  [bitcoinaddress message]
  {:pre [(string? bitcoinaddress)
         (string? message)]})


(defrpc signrawtransaction
  "(version 0.7) Adds signatures to a raw transaction and returns the
  resulting raw transaction.
  txinfo is of the form [{:txid \"txid\", :vout n,
                          :scriptPubKey \"hex\"}].
  Exact case is important, pay attention to scriptPubKey."
  [hexstring txinfo privatekeys]
  {:pre [(string? hexstring)
         (vector? txinfo)]})

(defrpc stop
  "Stop bitcoin server."
  [])

(defrpc submitblock
  "Attempts to submit new block to network."
  [hex data optional-params-obj])

(defrpc validateaddress
  "Return information about <bitcoinaddress>."
  [bitcoinaddress]
  {:pre [(string? bitcoinaddress)]})

(defrpc verifymessage
  "Verify a signed message."
  [bitcoinaddress signature message]
  {:pre [(string? bitcoinaddress)
         (string? signature)
         (string? message)]})

(defrpc walletlock
  "Removes the wallet encryption key from memory, locking the
  wallet. After calling this method, you will need to call
  walletpassphrase again before being able to call any methods which
  require the wallet to be unlocked."
  [])

(defrpc walletpassphrase
  "Stores the wallet decryption key in memory for <timeout> seconds."
  [passphrase timeout]
  {:pre [(string? passphrase)
         (number? timeout)]})

(defrpc walletpassphrasechange
  "Changes the wallet passphrase from <oldpassphrase> to <newpassphrase>."
  [oldpassphrase newpassphrase]
  {:pre [(string? oldpassphrase)
         (string? newpassphrase)]})
