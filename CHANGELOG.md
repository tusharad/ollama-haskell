# Revision history for ollama-haskell

## 0.1.2.0 -- 2024-11-20

* Added hostUrl and responseTimeOut options in generate function.
* Added hostUrl and responseTimeOut options in chat function.

## 0.1.1.3 -- 2024-11-08

* Increase response timeout to 15 minutes
* Added encodeImage utility function that converts image filePath to base64 image data.
* Added generateJson and chatJson. High level function to return response in Haskell type.

## 0.1.0.3 -- 2024-11-05

* Moving to stack instead of cabal.

## 0.1.0.2 -- 2024-10-18

* Increased response timeout time for chat function. 

## 0.1.0.1 -- 2024-10-18

* Renaming Lib.hs to OllamaExamples.hs as it was conflicting `Lib.hs` name

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
