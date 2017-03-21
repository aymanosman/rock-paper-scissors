{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Control.Exception
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent
import Network.Simple.TCP

type Name = String
type Score = Int
data Cmd =
  StartGame SockAddr
  | Choose SockAddr
  | Choosen Choice

data Choice = Rock | Paper | Scissors
  deriving (Show)

type Player = (Name, TChan Cmd, Score)
data Game = Game
  { players :: [(SockAddr, Player)]
  , waiting :: Maybe SockAddr
  -- , games :: []
  }

instance Show Game where
  show g =
    "Game " ++ show (map (\(_, (n, _, s)) -> (n, s)) (players g))

main :: IO ()
main = do
  t <- newTVarIO (Game [] Nothing)
  -- takeTMVar from waiting tv
  void $ forkIO $ forever $ do
    g <- readTVarIO t
    putStrLn $ "Game State: " ++ show g
    threadDelay (2000 * 1000)
  serve (Host "0.0.0.0") "8000" $ \(sock, remoteAddr) ->
    let
      setup = do
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        send sock "You are connected\n"
        c <- newTChanIO
        atomically $ modifyTVar t $ \g ->
          let ps = (remoteAddr, ("<anon>", c, 0)):players g
          in g { players = ps}
        return c

      cleanup = do
        putStrLn "Cleaning up..."
        atomically $ modifyTVar t $ \g ->
          let ps = filter ((/= remoteAddr) . fst) (players g)
          in g { players = ps}

      runPlayer c = do
        msg <- atomically $ readTChan c
        case msg of
          StartGame pAddr -> do
            -- let sock
            send sock $ BS.pack $ "A player has started a game with you " <> show pAddr <> "\n"
            putStrLn $ "got start game from " <> show pAddr
            c' <- atomically $ getPlayerChan pAddr t
            atomically $ writeTChan c' $ Choose remoteAddr
            choice <- readPlayerChoice sock
            send sock $ BS.pack $ "You chose " <> show choice
            runPlayer c

          Choose pAddr -> do
            choice <- readPlayerChoice sock
            case choice of
              Nothing ->
                return ()
              Just choice' -> do
                c' <- atomically $ getPlayerChan pAddr t
                atomically $ writeTChan c' $ Choosen choice'
                runPlayer c

          Choosen choice -> do
            send sock $ BS.pack $ "The other player chose " <> show choice
            runPlayer c

        return ()

      tryJoinGame c = do
        -- if waiting blah else print "waiting for game..."
        isWaiting <- atomically $ do
          g <- readTVar t
          case waiting g of
               Nothing -> do
                 writeTVar t $ g {waiting = Just remoteAddr}
                 return True
               Just s -> do
                 c' <- getPlayerChan s t
                 writeTChan c' $ StartGame remoteAddr
                 writeTVar t $ g {waiting = Nothing}
                 -- putStrLn $ "Matched with player " ++ n
                 -- send remoteAddr $ "Matched with player " ++ n
                 return False
        if isWaiting then
          send sock "You are waiting for a player...\n"
        else
          send sock "Joining you to game...\n"
        runPlayer c
    in
      (setup >>= tryJoinGame) `finally` cleanup


getPlayerChan :: SockAddr -> TVar Game -> STM (TChan Cmd)
getPlayerChan sockAddr tgame = do
  game <- readTVar tgame
  let ps = players game
  let [(_, (_, c, _))] = filter ((== sockAddr) . fst) ps
  return c

readPlayerChoice :: Socket -> IO (Maybe Choice)
readPlayerChoice sock  = do
  send sock "Choose (r)rock (p)aper (s)cissors\n"
  mreply <- recv sock 1024
  case mreply of
    Just "r\n" -> do
      putStrLn "got r"
      return $ Just Rock
    Just "p\n" -> do
      putStrLn "got p"
      return $ Just Paper
    Just "s\n" -> do
      putStrLn "got s"
      return $ Just Scissors
    Just _ -> do
      putStrLn "unrecogognisedasd asd as d"
      send sock "You'r shit, try again...\n"
      readPlayerChoice sock
    Nothing -> return Nothing
