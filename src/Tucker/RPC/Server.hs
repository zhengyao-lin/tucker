module Tucker.RPC.Server where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBSR
import qualified Data.ByteString.Base64 as B64

import Network.HTTP
import Network.HTTP.Auth

import Tucker.Enc
import Tucker.Util
import Tucker.Conf
import Tucker.Thread

import Tucker.P2P.Node
import Tucker.P2P.Init

import Tucker.RPC.HTTP
import Tucker.RPC.Protocol

import Tucker.Wallet.Wallet

encodeStrict :: JSON.ToJSON r => r -> ByteString
encodeStrict = LBSR.toStrict . JSON.encode

procRequest :: TCKRConf -> MainLoopEnv -> ByteString -> IO ByteString
procRequest conf env body =
    case JSON.eitherDecodeStrict body of
        Left err ->
            return $ encodeStrict $
            RPCResponse JSON.Null (RPCError RPCParseError err Nothing)
        
        Right req ->
            execCall conf env req (requestCall req)

mkJSONResponse :: ByteString -> Response ByteString
mkJSONResponse dat =
    mkResponse 200 "OK" [
        mkHeader HdrContentType "application/x-json"
    ] dat

serverRPC :: TCKRConf -> MainLoopEnv -> IO ()
serverRPC conf env =
    serverHTTP tstate (tckr_rpc_server_addr conf) (fi (tckr_rpc_server_port conf)) handler
    where
        tstate = envThreadState env

        unauth =
            mkResponse 401 "Unauthorized"
                       [ mkHeader HdrWWWAuthenticate "Basic realm=\"jsonrpc\"" ] mempty

        badreq = mkResponse 400 "Bad Request" [] mempty

        proc req =
            mkJSONResponse <$>
            procRequest conf env (rqBody req)

        -- a simple, specialized handler for bitcoin rpc purpose
        handler :: Request ByteString -> IO (Response ByteString)
        handler req =
            -- checking required basic access authorization
            case findHeader HdrAuthorization req of
                Nothing -> return unauth
                
                Just ('B':'a':'s':'i':'c':' ':value) ->
                    -- tLnM ("auth: " ++ value)

                    case B64.decode (BS.pack value) of
                        Left err -> return badreq
                        Right bs -> do
                            let str = BS.unpack bs
                                (user, pass) = splitOn ':' str
                            
                            if user == tckr_rpc_user_name conf &&
                               pass == tckr_rpc_user_pass conf then
                                proc req
                            else
                                return unauth

                _ -> return badreq

-- request handlers

suc :: JSON.ToJSON r => RPCRequest -> r -> IO ByteString
suc req r = return $
    encodeStrict (respondTo req (RPCSuccess r))

err :: RPCRequest -> RPCErrorCode -> String -> IO ByteString
err req code msg = return $
    encodeStrict (respondTo req (RPCError code msg Nothing))

execCall :: TCKRConf -> MainLoopEnv -> RPCRequest -> RPCCall -> IO ByteString
execCall conf env req RPCGetInfo = suc req "hello"

execCall conf env req (RPCGetBalance maddr) = do
    bal <- envWithWallet env $ \wal ->
        getBalance wal maddr

    case bal of
        [] -> err req RPCInternalError "wallet not enabled"
        b:_ -> suc req b

execCall conf env req (RPCUnknown method) =
    err req RPCMethodNotFound ("method " ++ show method ++ " does not exists")

-- execCall conf env req _ =
--     err req RPCInternalError "method not implemented"

{-

simpleHTTP (postRequestWithBody "http://tucker:sonia@127.0.0.1:3150/" "application/x-json" "{\"method\":\"getbalance\",\"id\":10086}")

-}

{-

AddMultiSigAddress: adds a P2SH multisig address to the wallet.
BackupWallet: safely copies wallet.dat to the specified file, which can be a directory or a path with filename.
DumpPrivKey: returns the wallet-import-format (WIP) private key corresponding to an address. (But does not remove it from the wallet.)
DumpWallet: creates or overwrites a file with all wallet keys in a human-readable format.
EncryptWallet: encrypts the wallet with a passphrase. This is only to enable encryption for the first time. After encryption is enabled, you will need to enter the passphrase to use private keys.
GetAccountAddress: returns the current Bitcoin address for receiving payments to this account. If the account doesn’t exist, it creates both the account and a new address for receiving payment. Once a payment has been received to an address, future calls to this RPC for the same account will return a different address. Deprecated
GetAccount: returns the name of the account associated with the given address.
GetAddressesByAccount: returns a list of every address assigned to a particular account. Deprecated
GetBalance: gets the balance in decimal bitcoins across all accounts or for a particular account.
GetNewAddress: returns a new Bitcoin address for receiving payments. If an account is specified, payments received with the address will be credited to that account.
GetRawChangeAddress: returns a new Bitcoin address for receiving change. This is for use with raw transactions, not normal use.
GetReceivedByAccount: returns the total amount received by addresses in a particular account from transactions with the specified number of confirmations. It does not count coinbase transactions. Deprecated
GetReceivedByAddress: returns the total amount received by the specified address in transactions with the specified number of confirmations. It does not count coinbase transactions.
GetTransaction: gets detailed information about an in-wallet transaction. Updated in 0.12.0
GetUnconfirmedBalance: returns the wallet’s total unconfirmed balance.
GetWalletInfo: provides information about the wallet.
ImportAddress: adds an address or pubkey script to the wallet without the associated private key, allowing you to watch for transactions affecting that address or pubkey script without being able to spend any of its outputs.
ImportPrunedFunds: imports funds without the need of a rescan. Meant for use with pruned wallets. New in 0.13.0
ImportPrivKey: adds a private key to your wallet. The key should be formatted in the wallet import format created by the dumpprivkey RPC.
ImportWallet: imports private keys from a file in wallet dump file format (see the dumpwallet RPC). These keys will be added to the keys currently in the wallet. This call may need to rescan all or parts of the block chain for transactions affecting the newly-added keys, which may take several minutes.
KeyPoolRefill: fills the cache of unused pre-generated keys (the keypool).
ListAccounts: lists accounts and their balances. Deprecated
ListAddressGroupings: lists groups of addresses that may have had their common ownership made public by common use as inputs in the same transaction or from being used as change from a previous transaction.
ListLockUnspent: returns a list of temporarily unspendable (locked) outputs.
ListReceivedByAccount: lists the total number of bitcoins received by each account. Deprecated
ListReceivedByAddress: lists the total number of bitcoins received by each address.
ListSinceBlock: gets all transactions affecting the wallet which have occurred since a particular block, plus the header hash of a block at a particular depth.
ListTransactions: returns the most recent transactions that affect the wallet. Updated in 0.12.1
ListUnspent: returns an array of unspent transaction outputs belonging to this wallet. Updated in 0.13.0
LockUnspent: temporarily locks or unlocks specified transaction outputs. A locked transaction output will not be chosen by automatic coin selection when spending bitcoins. Locks are stored in memory only, so nodes start with zero locked outputs and the locked output list is always cleared when a node stops or fails.
Move: moves a specified amount from one account in your wallet to another using an off-block-chain transaction. Deprecated
SendFrom: spends an amount from a local account to a bitcoin address. Deprecated
SendMany: creates and broadcasts a transaction which sends outputs to multiple addresses.
SendToAddress: spends an amount to a given address.
SetAccount: puts the specified address in the given account. Deprecated
SetTxFee: sets the transaction fee per kilobyte paid by transactions created by this wallet.
SignMessage: signs a message with the private key of an address.
WalletLock: removes the wallet encryption key from memory, locking the wallet. After calling this method, you will need to call walletpassphrase again before being able to call any methods which require the wallet to be unlocked.
WalletPassphrase: stores the wallet decryption key in memory for the indicated number of seconds. Issuing the walletpassphrase command while the wallet is already unlocked will set a new unlock time that overrides the old one.
WalletPassphraseChange: changes the wallet passphrase from ‘old passphrase’ to ‘new passphrase’.

-}
