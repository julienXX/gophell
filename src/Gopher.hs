{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Gopher where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           System.Directory
import           System.FilePath

data Config = Config {
  port     :: Int,
  user     :: String,
  hostname :: Text,
  content  :: FilePath
  }

type Listing = [Entry]

data EntryType = File | Directory | Gif | Image | Sound | Html | InfoLine | Binary | Error

data Entry = Entry {
  entryType :: EntryType,
  label     :: Text,
  uri       :: Text,
  dirHost   :: Text,
  dirPort   :: Int
  }

getListing :: Config -> IO Listing
getListing c = listDirectory (content c) >>= mapM (mkEntry c)

mkEntry :: Config -> FilePath -> IO Entry
mkEntry c fp = return Entry { entryType = filePathToEntryType fp
                            , label = T.pack $ dropExtensions filename
                            , uri = "/" `mappend` T.pack filename
                            , dirHost = hostname c
                            , dirPort = port c
                            }
  where
    filename = takeFileName fp


filePathToEntryType :: FilePath -> EntryType
filePathToEntryType fp =
  case takeExtension fp of
    ".txt"  -> File
    ".gif"  -> Gif
    ".png"  -> Image
    ".jpg"  -> Image
    ".pdf"  -> Binary
    ".wav"  -> Sound
    ".mp3"  -> Sound
    ".html" -> Html
    _       -> InfoLine

entryTypePrefix :: EntryType -> Text
entryTypePrefix e = case e of
  File      -> "0"
  Directory -> "1"
  Binary    -> "9"
  Gif       -> "g"
  Image     -> "I"
  Sound     -> "s"
  Html      -> "h"
  InfoLine  -> "i"
  _         -> "3"

isFile :: EntryType -> Bool
isFile Directory = False
isFile _         = True

renderListing :: Listing -> Text
renderListing = foldr renderEntry ""
  where
    renderEntry Entry{..} acc = acc `mappend`
      T.concat [ entryTypePrefix entryType,
                 label, "\t",
                 uri, "\t",
                 dirHost, "\t",
                 T.pack . show $ dirPort, "\r\n"
               ]
