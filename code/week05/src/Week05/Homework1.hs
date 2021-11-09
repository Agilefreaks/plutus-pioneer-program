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

module Week05.Homework1 where

import           Control.Monad          (Monad ((>>), (>>=)), void)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Default           (Default (..))
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Ledger                 (POSIXTime, PubKeyHash,
                                         ScriptContext (scriptContextTxInfo),
                                         TxInfo (txInfoValidRange), contains,
                                         mkMintingPolicyScript, pubKeyHash,
                                         scriptCurrencySymbol, to, txId,
                                         txSignedBy)
import           Ledger.Constraints     as Constraints (mintingPolicy,
                                                        mustMintValue,
                                                        mustValidateIn)
import           Ledger.TimeSlot        (slotToBeginPOSIXTime)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value (CurrencySymbol, TokenName,
                                                  singleton)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Plutus.Contract        as Contract (Contract, Endpoint,
                                                     awaitTxConfirmed,
                                                     currentTime, endpoint,
                                                     logError, logInfo,
                                                     ownPubKey,
                                                     submitTxConstraintsWith)
import           Plutus.Trace.Emulator  as Emulator (activateContractWallet,
                                                     callEndpoint,
                                                     runEmulatorTraceIO,
                                                     waitNSlots)
import qualified PlutusTx
import           PlutusTx.Prelude       (AdditiveSemigroup ((+)), Bool, Integer,
                                         Ord ((>)), traceIfFalse, ($), (&&),
                                         (<$>))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
-- This policy should only allow minting (or burning) of tokens if the owner of the specified PubKeyHash
-- has signed the transaction and if the specified deadline has not passed.
mkPolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx = traceIfFalse "You must be the owner" isOwner &&
                               traceIfFalse "Deadline has passed" beforeDeadline
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        isOwner :: Bool
        isOwner = txSignedBy info pkh

        beforeDeadline :: Bool
        beforeDeadline = contains (to deadline) (txInfoValidRange info)

policy :: PubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

curSymbol :: PubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !POSIXTime
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    now <- Contract.currentTime
    let deadline = mpDeadline mp
    if now > deadline
        then Contract.logError @String "deadline passed"
        else do
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.mintingPolicy $ policy pkh deadline
                tx      = Constraints.mustMintValue val <> Constraints.mustValidateIn (to $ now + 5000)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = slotToBeginPOSIXTime def 10
    h <- activateContractWallet (Wallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 15
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1
