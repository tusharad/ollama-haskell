module Main (main) where

-- import qualified Example.KnowledgeApp as KA
import qualified Example.ChatWithToolCall as ToolCall

main :: IO ()
main = do 
  ToolCall.runApp
