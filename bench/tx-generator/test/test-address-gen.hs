{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Prelude
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import Cardano.Api

keyAddress :: NetworkId -> VerificationKey PaymentKey -> AddressInEra MaryEra
keyAddress networkId k
  = makeShelleyAddressInEra
      networkId
      (PaymentCredentialByKey $ verificationKeyHash k)
      NoStakeAddress


-- readTextEnvelopeFromFile "/tmp/Downloads/tf"
keyEnvelope :: TextEnvelope
keyEnvelope = TextEnvelope
  "GenesisUTxOSigningKey_ed25519"
  "Genesis Initial UTxO Signing Key"
  "X \ETX|\153m\216\t(\228\250\n6M`X\213\f\190\223\225MX\192\v0\170\FS\213\174:w\150g"

testKey :: SigningKey PaymentKey
testKey = case deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) keyEnvelope of
  Right k -> castSigningKey k
  Left err -> error $ show err

main :: IO ()
main = do
  let
    verificationKey = getVerificationKey testKey
    expectedAddress = keyAddress (Testnet $ NetworkMagic 42) verificationKey
  putStrLn $ show testKey
  putStrLn $ show expectedAddress
  good <- newMVar 0
  bad <- newMVar 0
  _ <- forkIO $ runner good bad verificationKey expectedAddress
  _ <- forkIO $ runner good bad verificationKey expectedAddress
  forever $ do
    g <- readMVar good
    b <- readMVar bad
    putStrLn $ show (g,b)
    threadDelay 1000000

runner :: MVar Integer -> MVar Integer -> VerificationKey PaymentKey -> AddressInEra MaryEra -> IO ()
runner good bad key expectedAddress = forever $ do
  addr <- do
    return $ keyAddress (Testnet $ NetworkMagic 42) key
  if addr == expectedAddress
    then modifyMVar_ good (return . succ)
    else modifyMVar_ bad (return . succ)
