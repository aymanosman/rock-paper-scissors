{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Network.Simple.TCP

type Name = String
type Score = Int
type Player = (Name, Score)
data Game = Game
  { players :: [(SockAddr, Player)]
  } deriving (Show)

main :: IO ()
main = do
  t <- newTVarIO (Game [])
  void $ forkIO $ forever $ do
    g <- readTVarIO t
    putStrLn $ "Game State: " ++ show g
    threadDelay (2000 * 1000)
  serve (Host "0.0.0.0") "8000" $ \(sock, remoteAddr) ->
    let
      setup = do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        send sock "hello\n"
        atomically $ modifyTVar t (\(Game ps) -> Game ((remoteAddr, ("<anon>", 0)):ps))
        -- joinGame
      cleanup =
        atomically $ modifyTVar t (\(Game ps) -> Game (filter ((/= remoteAddr) . fst) ps))
      play = forever $ do
        send sock "Hey there\n"
        threadDelay (1000 * 1000)
    in
    setup >> play `finally` cleanup
