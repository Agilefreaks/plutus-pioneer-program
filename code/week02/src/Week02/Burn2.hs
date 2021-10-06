{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Burn2 where

import           Control.Monad               (void, (>>), (>>=))
import           Data.Text                   (Text)
import           GHC.Base                    (Monoid (mconcat), String, ($),
                                              (<>))
import           GHC.Num                     (Integer)
import           Ledger                      (Address, Datum (Datum),
                                              Redeemer (Redeemer), Validator,
                                              ValidatorHash, scriptAddress,
                                              txId)
import           Ledger.Ada                  as Ada (lovelaceValueOf)
import           Ledger.Scripts              (mkValidatorScript)
import qualified Ledger.Scripts              as Scripts

import           Data.Functor                ((<$>))
import qualified Data.Map                    as Map
import           Data.Tuple                  (fst)
import           Data.Void                   (Void)
import qualified Ledger.Constraints          as Constraints
import           Playground.TH               (mkKnownCurrencies,
                                              mkSchemaDefinitions)
import           Playground.Types            (KnownCurrency (..))
import           Plutus.Contract             (AsContractError, Contract,
                                              Endpoint, awaitTxConfirmed,
                                              endpoint, logInfo, select,
                                              submitTx, submitTxConstraintsWith,
                                              type (.\/), utxoAt)
import           Plutus.Contract.Constraints (TxConstraints,
                                              mustPayToOtherScript,
                                              mustSpendScriptOutput)
import           PlutusTx                    (Data (Constr, I), compile)
import           PlutusTx.Prelude            (traceError)
import           Text.Printf                 (printf)

-- Needed for playground
import           Playground.Contract         (ensureKnownCurrencies, printJson,
                                              printSchemas, stage)
import           Prelude                     (IO)


-- Datum -> Redeemer -> Context
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = traceError "Burnt!"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- offchain

type GiftSchema =
        Endpoint "give" Integer
    .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelance" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxoAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collect gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >> grab

mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies []
