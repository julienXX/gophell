{-# LANGUAGE OverloadedStrings #-}
module GopherSpec where

import           Data.Text
import           Gopher
import           Test.Hspec

spec :: Spec
spec = describe "renderListing" $ do

  let file      = Entry { entryType = File, label = "file", uri = "/file", dirHost = "some dir", dirPort = 70 }
  let directory = Entry { entryType = Directory, label = "dir", uri = "/dir", dirHost = "some dir", dirPort = 70 }
  let gif       = Entry { entryType = Gif, label = "image.gif", uri = "/image", dirHost = "some dir", dirPort = 70 }
  let image     = Entry { entryType = Image, label = "image.jpg", uri = "/image", dirHost = "some dir", dirPort = 70 }
  let sound     = Entry { entryType = Sound, label = "sound.wav", uri = "/sound", dirHost = "some dir", dirPort = 70 }
  let html      = Entry { entryType = Html, label = "page.html", uri = "/page", dirHost = "some dir", dirPort = 70 }
  let infoline  = Entry { entryType = InfoLine, label = "info line", uri = "/info line", dirHost = "some dir", dirPort = 70 }
  let binary    = Entry { entryType = Binary, label = "binary", uri = "/binary", dirHost = "some dir", dirPort = 70 }

  it "renders nothing" $
    renderListing [] `shouldBe` ""

  it "renders a file" $
    renderListing [file] `shouldBe` "0file\t/file\tsome dir\t70\r\n"

  it "renders a directory" $
    renderListing [directory] `shouldBe` "1dir\t/dir\tsome dir\t70\r\n"

  it "renders a gif" $
    renderListing [gif] `shouldBe` "gimage.gif\t/image\tsome dir\t70\r\n"

  it "renders an image" $
    renderListing [image] `shouldBe` "Iimage.jpg\t/image\tsome dir\t70\r\n"

  it "renders a sound" $
    renderListing [sound] `shouldBe` "ssound.wav\t/sound\tsome dir\t70\r\n"

  it "renders a html file" $
    renderListing [html] `shouldBe` "hpage.html\t/page\tsome dir\t70\r\n"

  it "renders an info line" $
    renderListing [infoline] `shouldBe` "iinfo line\t/info line\tsome dir\t70\r\n"

  it "renders a binary" $
    renderListing [binary] `shouldBe` "9binary\t/binary\tsome dir\t70\r\n"

  it "renders a file, an image and a directory" $
    renderListing [file, image, directory] `shouldBe`
      "1dir\t/dir\tsome dir\t70\r\nIimage.jpg\t/image\tsome dir\t70\r\n0file\t/file\tsome dir\t70\r\n"
