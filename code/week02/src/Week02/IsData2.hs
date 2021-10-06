{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.IsData2 where

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
import           PlutusTx                    (Data (I), IsData (toData),
                                              compile, unstableMakeIsData)
import           PlutusTx.Prelude            (Bool, Eq ((==)), traceIfFalse)
import           Text.Printf                 (printf)

-- Needed for playground
import qualified Ledger.Typed.Scripts        as Scripts
import           Playground.Contract         (ensureKnownCurrencies, printJson,
                                              printSchemas, stage)
import           Prelude                     (IO)

newtype MySillyRedeemer = MySillyRedeemer Integer

PlutusTx.unstableMakeIsData ''MySillyRedeemer

-- Datum -> Redeemer -> Context
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator _ (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = MySillyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator  @() @MySillyRedeemer

validator :: Validator
validator = Scripts.validatorScript  typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- offchain

type GiftSchema =
        Endpoint "give" Integer
    .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelance" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab n = do
    utxos <- utxoAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData (MySillyRedeemer n) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collect gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema
mkKnownCurrencies []
