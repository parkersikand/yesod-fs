{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dispatch where

import Yesod
import Foundation
import Handler.Home
import Handler.Preview
import Handler.Download

mkYesodDispatch "App" resourcesApp