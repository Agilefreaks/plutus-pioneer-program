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

module Week05.Homework2 where

import           Control.Monad          (Monad ((>>)), void)
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
import           Plutus.V1.Ledger.Api   (TokenName (TokenName))
import qualified PlutusTx
import           PlutusTx.Prelude       (Bool (False), Eq ((==)), any,
                                         emptyByteString, traceIfFalse, ($),
                                         (&&), (.))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet (Wallet (Wallet))

{-# INLINABLE tn #-}
tn :: TokenName
tn = TokenName emptyByteString

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy oref () ctx = traceIfFalse "must consum UTxO" consumesOutput &&
                       traceIfFalse "must have correct value" isValueCorrect
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      consumesOutput :: Bool
      consumesOutput = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

      isValueCorrect :: Bool
      isValueCorrect = case flattenValue (txInfoForge info) of
        [(_, tn', amt)] -> amt == 1 && tn' == tn
        _               -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema = Endpoint "mint" ()

mint :: Contract w NFTSchema Text ()
mint = do
  pk <- Contract.ownPubKey
  utxos <- utxoAt (pubKeyAddress pk)
  case Map.keys utxos of
    [] -> Contract.logError @String "No utxos found"
    utxo:_ -> do
      let val = Value.singleton (curSymbol utxo) "" 1
          lookups = Constraints.mintingPolicy (policy utxo) <> Constraints.unspentOutputs utxos
          tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput utxo
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "Minted %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >> mint

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 ()
    callEndpoint @"mint" h2 ()
    void $ Emulator.waitNSlots 1
