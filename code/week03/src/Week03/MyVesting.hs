{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE MultiParamTypeClasses #-}
module Week03.MyVesting where

import           Data.Aeson.Types                (FromJSON, ToJSON)
import           Data.Functor                    (void)
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import           Data.Void                       (Void)
import           Ledger                          (Address, Datum (Datum),
                                                  PubKey, Redeemer (Redeemer),
                                                  TxInfo (TxInfo, txInfoValidRange),
                                                  ValidatorHash, contains,
                                                  pubKeyHash, scriptAddress,
                                                  txId, txSignedBy)
import           Ledger.Constraints              (TxConstraints,
                                                  mustPayToTheScript,
                                                  mustSpendScriptOutput,
                                                  mustValidateIn)
import qualified Ledger.Constraints              as Constraints
import qualified Ledger.Typed.Scripts.Validators as Scripts
import           Playground.Contract             (AsContractError, Generic, IO,
                                                  KnownCurrency, ToSchema,
                                                  endpoint,
                                                  ensureKnownCurrencies,
                                                  mkKnownCurrencies,
                                                  mkSchemaDefinitions,
                                                  ownPubKey, printSchemas,
                                                  utxoAt)
import           Plutus.Contract                 (currentTime, logInfo, select,
                                                  type (.\/))
import           Plutus.Contract.Request         (Endpoint, awaitTxConfirmed,
                                                  submitTxConstraints,
                                                  submitTxConstraintsWith)
import           Plutus.Contract.Types           (Contract (Contract))
import qualified Plutus.V1.Ledger.Ada            as Ada
import           Plutus.V1.Ledger.Api            (PubKeyHash (PubKeyHash),
                                                  ScriptContext (scriptContextTxInfo),
                                                  Validator)
import           Plutus.V1.Ledger.Interval       (from)
import           Plutus.V1.Ledger.Time           (POSIXTime (POSIXTime))
import           Plutus.V1.Ledger.Tx             (Tx (txData),
                                                  TxOut (txOutDatumHash),
                                                  TxOutTx (TxOutTx, txOutTxOut, txOutTxTx))
import qualified PlutusTx
import qualified PlutusTx.IsData                 as PlutusTx
import           PlutusTx.Prelude                (Bool (False), Eq ((==)),
                                                  Integer,
                                                  Maybe (Just, Nothing),
                                                  Ord ((<), (<=)), fst, mconcat,
                                                  traceIfFalse, ($), (&&), (.),
                                                  (<$>), (>>), (>>=))
import           Prelude                         (Bool (False), Integer,
                                                  Semigroup ((<>)), Show (show),
                                                  String, undefined)
import           Text.Printf                     (printf)

data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.makeLift ''VestingParam

{-# INLINABLE mkValidator #-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx = traceIfFalse "bereficiary's signature is missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ beneficiary p

        deadlineReached :: Bool
        deadlineReached = contains (from $ deadline p) (txInfoValidRange info)

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    Endpoint "give" GiveParams
    .\/ Endpoint "grab" POSIXTime

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let p = VestingParam { beneficiary = gpBeneficiary gp, deadline = gpDeadline gp }
        tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelance to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
    now <- currentTime
    pkh <- pubKeyHash <$> ownPubKey
    if now < d
        then logInfo @String $ "too early"
        else do
            let p = VestingParam
                        { beneficiary = pkh
                        , deadline = d
                        }
            utxos <- utxoAt $ scrAddress p
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    let orefs = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos <> Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <> mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
    where give' = endpoint @"give" >>= give
          grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''VestingSchema
mkKnownCurrencies []
