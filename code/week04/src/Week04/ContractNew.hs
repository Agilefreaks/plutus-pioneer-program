{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Week04.ContractNew where

import Plutus.Contract as Contract ( logInfo, Contract, Empty, throwError, handleError, logError, Endpoint, endpoint, type (.\/), waitNSlots, tell )
import Data.Text (Text, unpack)
import Control.Monad (void)
import Data.Void (Void)
import Plutus.Trace.Emulator  as Emulator (activateContractWallet, runEmulatorTraceIO, EmulatorTrace, callEndpoint, observableState, waitNSlots)
import Wallet.Emulator (Wallet(Wallet))
import Control.Monad.Freer.Extras as Extras ( logInfo )

-- Contract w s e a

myContract1 :: Contract () Empty Text ()
myContract1 = do
    void $ Contract.throwError "BOOM"
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () Empty Void ()
myContract2 = do
    Contract.handleError (\e -> Contract.logError $ "caught: " ++ unpack e) myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @"foo"
    Contract.logInfo n
    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots  10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4
    
    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
