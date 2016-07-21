{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import System.Environment (lookupEnv)
import Web.Slack

data CounterState = CounterState
                  { _messageCount :: Int
                  }

makeLenses ''CounterState

main :: IO ()
main = do
    conf <- mkConfig
    runSlack conf $ evalStateT counterBot (CounterState 0)

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

-- Count how many messages the bot recieves
counterBot :: StateT CounterState Slack ()
counterBot = forever $
    getNextEvent >>= \case
        Message cid _ _ _ _ _ -> do
            num <- messageCount <%= (+1)
            sendMessage cid (T.pack . show $ num)
        _ -> return ()
