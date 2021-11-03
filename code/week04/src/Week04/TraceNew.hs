{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Week04.TraceNew where

import           Control.Monad.Freer.Extras as Extras (logInfo)
import           Plutus.Trace.Emulator      as Emulator (EmulatorTrace,
                                                         activateContractWallet,
                                                         callEndpoint,
                                                         runEmulatorTraceIO,
                                                         waitNSlots,
                                                         waitUntilSlot)

import           Control.Monad              (void)
import           Data.Default               (Default (def))
import           Ledger                     (pubKeyHash)
import           Ledger.TimeSlot            (slotToBeginPOSIXTime)
import           Playground.Contract        (Wallet (Wallet))
import           Plutus.Contract.Trace      (walletPubKey)
import           Plutus.V1.Ledger.Slot      (Slot (Slot))
import           Week04.Vesting             (GiveParams (GiveParams, gpAmount, gpBeneficiary, gpDeadline),
                                             endpoints)

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline = slotToBeginPOSIXTime def $ Slot 20
        , gpAmount = 10000000 }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached: " ++ show s


