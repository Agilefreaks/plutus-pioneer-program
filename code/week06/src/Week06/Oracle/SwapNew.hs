{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.SwapNew where

import           Control.Monad         (guard)
import           Data.List             (find)
import qualified Data.Map              as Map
import           Data.Maybe            (mapMaybe)
import           Data.Monoid           (Last (..))
import           Data.Text             (Text)
import           Ledger                (Address, Datum (Datum), PubKeyHash,
                                        Redeemer (Redeemer),
                                        ScriptContext (scriptContextTxInfo),
                                        Tx (txData),
                                        TxInInfo (txInInfoResolved),
                                        TxInfo (txInfoInputs),
                                        TxOut (txOutAddress, txOutDatumHash, txOutValue),
                                        TxOutRef,
                                        TxOutTx (txOutTxOut, txOutTxTx),
                                        Validator, findDatum, findOwnInput,
                                        pubKeyHash, scriptAddress,
                                        toValidatorHash, txId, txSignedBy,
                                        validatorHash, valuePaidTo)
import           Ledger.Ada            as Ada (Ada (getLovelace), fromValue,
                                               lovelaceValueOf)
import           Ledger.Constraints    as Constraints (mustPayToOtherScript,
                                                       mustPayToPubKey,
                                                       mustPayToTheScript,
                                                       mustSpendScriptOutput,
                                                       otherScript,
                                                       unspentOutputs)
import qualified Ledger.Typed.Scripts  as Scripts
import           Ledger.Value          as Value (Value, assetClassValue,
                                                 assetClassValueOf)
import           Plutus.Contract       as Contract (Contract, Endpoint,
                                                    awaitTxConfirmed, endpoint,
                                                    handleError, logError,
                                                    logInfo, ownPubKey, select,
                                                    submitTxConstraints,
                                                    submitTxConstraintsWith,
                                                    tell, type (.\/), utxoAt)
import qualified PlutusTx
import           PlutusTx.Prelude      (Bool, Eq ((==)), Integer, Maybe (..),
                                        MultiplicativeSemigroup ((*)),
                                        Ord ((<=), (>=)), divide, filter,
                                        isJust, length, mconcat, return,
                                        traceError, traceIfFalse, ($), (&&),
                                        (++), (.), (/=), (>>), (||))
import           Prelude               (Semigroup (..), Show (..), String,
                                        (<$>))

import           Week06.Oracle.CoreNew (Oracle (oAsset, oFee),
                                        OracleRedeemer (Use), findOracle,
                                        oracleAddress, oracleValidator,
                                        oracleValue)
import           Week06.Oracle.Funds   (ownFunds)

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelance exchangeRate = (lovelance * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
    traceIfFalse "price not paid" sellerPaid)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        oracleInput :: TxOut
        oracleInput =
            let
                ins = [o | i <- txInfoInputs info, let o = txInInfoResolved i, txOutAddress o == addr]
            in
                case ins of
                    [o] -> o
                    _   -> traceError "oracle value not found"

        oracleValue' = case oracleValue oracleInput (`findDatum` info) of
            Nothing -> traceError "oracle value not found"
            Just x  -> x

        hasTwoScriptInputs :: Bool
        hasTwoScriptInputs =
            let
                xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
            in
                length xs == 2

        minPrice :: Integer
        minPrice =
            let
                lovelaceIn = case findOwnInput ctx of
                    Nothing -> traceError "own input not found"
                    Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
            in
                price lovelaceIn oracleValue'

        sellerPaid :: Bool
        sellerPaid =
            let
                pricePaid :: Integer
                pricePaid = assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
            in
                pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
    where
        f :: TxOutTx -> Maybe PubKeyHash
        f o = do
            dh <- txOutDatumHash $ txOutTxOut o
            (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
            PlutusTx.fromBuiltinData  d

        g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
        g (oref, o) = do
            pkh <- f o
            guard $ p pkh
            return (oref, o, pkh)

retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _ -> do
            let tx = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]
                lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrived " ++ show (length xs) ++ " swap(s)"

useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf  funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh <- pubKeyHash <$> ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing -> logInfo @String "no suitable swaps found"
                Just (oref', o', pkh') -> do
                    let v = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        tx = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Use) <>
                             Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ()) <>
                             Constraints.mustPayToOtherScript (validatorHash $ oracleValidator oracle) (Datum $ PlutusTx.toBuiltinData x) v <>
                             Constraints.mustPayToPubKey pkh' p
                        lookups = Constraints.otherScript (swapValidator oracle) <>
                                  Constraints.otherScript (oracleValidator oracle) <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show p
    where
        getPrice :: Integer -> TxOutTx -> Integer
        getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

        f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
        f amt x (_, o, _) = getPrice x o <= amt

type SwapSchema =
    Endpoint "offer" Integer
    .\/ Endpoint "retrieve" ()
    .\/ Endpoint "use" ()
    .\/ Endpoint "funds" ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
    where
        offer :: Contract (Last Value) SwapSchema Text ()
        offer = h $ do
            amt <- endpoint @"offer"
            offerSwap oracle amt

        retrieve :: Contract (Last Value) SwapSchema Text ()
        retrieve = h $ do
            endpoint @"retrieve"
            retrieveSwaps oracle

        use :: Contract (Last Value) SwapSchema Text ()
        use = h $ do
            endpoint @"use"
            useSwap oracle

        funds :: Contract (Last Value) SwapSchema Text ()
        funds = h $ do
            endpoint @"funds"
            v <- ownFunds
            tell $ Last $ Just v

        h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
        h = handleError logError
