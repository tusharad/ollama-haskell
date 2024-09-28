{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ollama_haskell (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ollama_haskell"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Ollama Haskell library"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
