{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Prelude
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad

import Cardano.Api

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
  let expectedVKey = getVerificationKey testKey
  putStrLn $ show testKey
  putStrLn $ show expectedVKey
  good <- newMVar 0
  bad <- newMVar 0
  _ <- forkIO $ runner good bad testKey expectedVKey
  _ <- forkIO $ runner good bad testKey expectedVKey
  forever $ do
    g <- readMVar good
    b <- readMVar bad
    putStrLn $ show (g,b)
    threadDelay 1000000

runner :: MVar Integer -> MVar Integer -> SigningKey PaymentKey -> VerificationKey PaymentKey -> IO ()
runner good bad key expectedAddress = forever $ do
  addr <- do
    return $ getVerificationKey key
  if addr == expectedAddress
    then modifyMVar_ good (return . succ)
    else modifyMVar_ bad (return . succ)
