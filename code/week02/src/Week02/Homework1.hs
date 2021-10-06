{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Homework1 where

import           Control.Monad               (void, (>>), (>>=))
import           Data.Text                   (Text)
import           GHC.Base                    (Monoid (mconcat), String, ($),
                                              (<>))
import           GHC.Num                     (Integer)
import           Ledger                      (Address, Redeemer (Redeemer),
                                              ScriptContext, Validator,
                                              scriptAddress, txId)
import           Ledger.Ada                  as Ada (lovelaceValueOf)

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
                                              submitTxConstraints,
                                              submitTxConstraintsWith,
                                              type (.\/), utxoAt)
import           Plutus.Contract.Constraints (TxConstraints, mustPayToTheScript,
                                              mustSpendScriptOutput)
import           PlutusTx                    (IsData (toData), compile)
import           PlutusTx.Prelude            (Bool, traceIfFalse, (&&))
import           Text.Printf                 (printf)

-- Needed for playground
import qualified Ledger.Typed.Scripts        as Scripts
import           Playground.Contract         (ensureKnownCurrencies, printJson,
                                              printSchemas, stage)
import           Prelude                     (IO)

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator _ (b, b') _ = traceIfFalse "There are not both TRUE" b && b'

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = (Bool, Bool)

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @(Bool, Bool)

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" (Bool, Bool)

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => (Bool, Bool) -> Contract w s e ()
grab bs = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData bs | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
