{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Dispatch ()
import Foundation
import qualified Data.IntMap as IM

import Database.MongoDB

import Control.Concurrent.STM

main :: IO ()
main = do
  conn <- runIOE $ connect (host "127.0.0.1")
  warpEnv $ App conn