{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Functor          (void)
import           Data.Text             (Text, unpack)
import           GHC.Generics          (Generic)
import           Ledger                (PubKeyHash, pubKeyHash)
import           Ledger.Ada            as Ada (lovelaceValueOf)
import           Ledger.Constraints    as Constraints (mustPayToPubKey)
import           Playground.Contract   (Wallet (Wallet))
import           Plutus.Contract       as Contract (Contract, Endpoint,
                                                    endpoint, handleError,
                                                    logError, submitTx)
import           Plutus.Contract.Trace (walletPubKey)
import           Plutus.Trace.Emulator as Emulator (EmulatorTrace,
                                                    activateContractWallet,
                                                    callEndpoint,
                                                    runEmulatorTraceIO,
                                                    waitNSlots)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    handleError (\e -> Contract.logError $ "caught: " ++ unpack e) $ void $ submitTx tx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace amount amount' = do
    h1 <- activateContractWallet (Wallet 1) payContract
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace = amount
        }
    void $ waitNSlots 1
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace = amount'
        }
    void $ waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
