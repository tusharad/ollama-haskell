{-# LANGUAGE OverloadedStrings #-}

module Example.List (runApp) where

import Data.Ollama.List
import qualified Data.Text.IO as T

runApp :: IO ()
runApp = do
  eList <- list Nothing
  case eList of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right (Models modelInfoList) -> do
      mapM_ (T.putStrLn . name) modelInfoList
