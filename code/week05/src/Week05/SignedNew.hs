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

module FreeNew where

import           Control.Monad           (Monad ((>>), (>>=)), void)
import           Data.Aeson              (ToJSON)
import           Data.Aeson.Types        (FromJSON)
import           Data.Text               (Text)
import           Data.Void               (Void)
import           GHC.Base                (IO, String, (.))
import           GHC.Generics            (Generic)
import           GHC.Show                (Show (show))
import           Ledger                  (CurrencySymbol, PubKeyHash,
                                          ScriptContext (scriptContextTxInfo),
                                          TokenName, mkMintingPolicyScript,
                                          pubKeyHash, scriptCurrencySymbol,
                                          txId, txSignedBy)
import qualified Ledger.Constraints      as Constraints
import qualified Ledger.Typed.Scripts    as Scripts
import           Playground.Contract     (KnownCurrency, Wallet (Wallet))
import           Playground.TH           (mkKnownCurrencies,
                                          mkSchemaDefinitions)
import           Plutus.Contract         (Contract, awaitTxConfirmed, endpoint,
                                          submitTxConstraintsWith)
import qualified Plutus.Contract         as Contract
import           Plutus.Contract.Request (Endpoint)
import           Plutus.Trace.Emulator   (activateContractWallet, callEndpoint,
                                          runEmulatorTraceIO)
import qualified Plutus.Trace.Emulator   as Emulator
import qualified Plutus.V1.Ledger.Value  as Value
import qualified PlutusTx
import           PlutusTx.Prelude        (Bool, Integer, ($), (<$>))
import           Schema                  (ToSchema)
import           Text.Printf             (printf)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let val = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
    where
        mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema
mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount = -222
        }
    void $ Emulator.waitNSlots 1
