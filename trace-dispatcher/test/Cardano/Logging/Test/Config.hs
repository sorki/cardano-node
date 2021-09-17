{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Logging.Test.Config (
  ) where

import           Data.Map (fromList)
import           Test.QuickCheck

import           Cardano.Logging


-- | different configurations for testing
config1 :: TraceConfig
config1 = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ ConfSeverity DebugF
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    ]
  }

config2 :: TraceConfig
config2 = emptyTraceConfig {
  tcOptions = fromList
    [ ([] :: Namespace,
         [ ConfSeverity DebugF
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    , (["Node", "Test", "Message1"],
         [ ConfSeverity InfoF
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured, EKGBackend]
         ])
    , (["Node", "Test", "Message2"],
         [ ConfSeverity ErrorF
         , ConfDetail DMinimal
         , ConfBackend [Forwarder, EKGBackend]
         ])
    ]
  }

instance Arbitrary TraceConfig where
  arbitrary = elements [config1, config2]
