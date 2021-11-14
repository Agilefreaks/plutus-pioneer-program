{-# LANGUAGE TypeApplications #-}

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    (Monad (return, (>>=)), void)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Ledger           (TxOut (txOutValue), TxOutTx (txOutTxOut),
                                   Value, pubKeyAddress)
import           Ledger.Value     as Value (flattenValue)
import           Plutus.Contract  as Contract (Contract, Empty, handleError,
                                               logError, logInfo, ownPubKey,
                                               tell, utxoAt, waitNSlots)
import           PlutusTx.Prelude (Maybe (Just), mconcat, ($), (++), (.))
import           Prelude          (Show (..), String, (<$>))

ownFunds :: Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
