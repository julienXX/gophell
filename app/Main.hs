{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent        (forkFinally)
import           Control.Exception         (bracket)
import           Control.Monad             (forever, void, when)
import           Data.ByteString           as BS hiding (foldr)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as E
import qualified Data.Text.IO              as T
import           Gopher
import           Network.Socket            hiding (bindSocket, recv)
import           Network.Socket.ByteString (recv, sendAll)
import           System.Directory
import           System.FilePath
import           System.Posix.User

main :: IO ()
main = bracket (bindSocket c) close (handleRequests c)
  where
    c = Config{ port=70, user="julien", hostname="localhost", content="/home/julien/Code/gophell/content/" }

handleRequests :: Config -> Socket -> IO()
handleRequests c s = forever $ do
  (conn, _) <- accept s
  void $ forkFinally (respond c conn) (\_ -> close conn)

respond :: Config -> Socket -> IO()
respond c s = recv s 1024 >>= sendResponse c >>= sendAll s

sendResponse :: Config -> BS.ByteString -> IO BS.ByteString
sendResponse c req = do
  let uri = takeFileName . T.unpack . Prelude.head . T.splitOn "\r\n" $ E.decodeUtf8 req
  file <- doesFileExist (content c </> uri)
  dir <- doesDirectoryExist (content c </> uri)
  resp <- if file
    then T.readFile (content c </> uri)
    else renderListing <$> getListing c
  return $ E.encodeUtf8 resp `mappend` "\r\n."

dropPrivileges :: Config -> IO ()
dropPrivileges c = do
  uid <- getRealUserID
  when (uid /= 0) $ return ()
  u <- getUserEntryForName $ user c
  setGroupID $ userGroupID u
  setUserID $ userID u

bindSocket :: Config -> IO Socket
bindSocket c = do
  addr <- resolveLocal . show $ port c
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  dropPrivileges c
  listen sock 10
  return sock
  where
    resolveLocal port = do
      let hints = defaultHints {
            addrFlags = [AI_PASSIVE],
            addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr
