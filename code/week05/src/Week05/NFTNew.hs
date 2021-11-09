{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFTNew where

import           Control.Monad          (Monad ((>>), (>>=)), void)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 (CurrencySymbol,
                                         ScriptContext (scriptContextTxInfo),
                                         TokenName, TxInInfo (txInInfoOutRef),
                                         TxInfo (txInfoForge, txInfoInputs),
                                         TxOutRef, mkMintingPolicyScript,
                                         pubKeyAddress, scriptCurrencySymbol,
                                         txId)
import           Ledger.Constraints     as Constraints (mintingPolicy,
                                                        mustMintValue,
                                                        mustSpendPubKeyOutput,
                                                        unspentOutputs)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value (flattenValue, singleton)
import           Plutus.Contract        as Contract (Contract, Endpoint,
                                                     awaitTxConfirmed, endpoint,
                                                     logError, logInfo,
                                                     ownPubKey,
                                                     submitTxConstraintsWith,
                                                     utxoAt)
import           Plutus.Trace.Emulator  as Emulator (activateContractWallet,
                                                     callEndpoint,
                                                     runEmulatorTraceIO,
                                                     waitNSlots)
import qualified PlutusTx
import           PlutusTx.Prelude       (Bool (False), Eq ((==)), any,
                                         traceIfFalse, ($), (&&))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet (Wallet (Wallet))

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"  hasUTxO &&
                          traceIfFalse "wrong amount minted" checkMintedAmmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmmount :: Bool
        checkMintedAmmount = case flattenValue (txInfoForge info) of
            [(_, tn', amt)] ->  tn' == tn && amt == 1
            _               -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

type NFTSchema = Endpoint "mint" TokenName

mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pk <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        [] -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
    where
        mint' = endpoint @"mint" >>= mint

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 tn
    callEndpoint @"mint" h2 tn
    void $ Emulator.waitNSlots 1
