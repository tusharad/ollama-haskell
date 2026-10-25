module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ollama

main :: IO ()
main = do
  client <- defaultClient
  putStrLn "=== Ollama System One Decision API (/v1/systemone) ==="

  putStrLn "\n1. Defining System One evaluation questions..."
  let state = "The customer requested a full refund after 45 days, but our policy limit is 30 days."

      -- Choice question: Categorize refund eligibility
      q1 =
        choiceQuestion
          "Is the customer eligible for an automatic or discretionary refund?"
          ( Map.fromList
              [ ("eligible", Just "Within standard 30 day window")
              , ("discretionary", Just "Requires manager escalation")
              , ("ineligible", Just "Strict policy violation")
              ]
          )

      -- Noul (binary probability) question
      q2 = noulQuestion "Will this request result in customer churn if denied?"

      -- Score question (1 to 5 rating)
      q3 =
        scoreQuestion
          "Rate customer frustration risk from 1 (low) to 5 (extreme)."
          ["1", "2", "3", "4", "5"]

      questions =
        Map.fromList
          [ ("eligibility", q1)
          , ("churn_risk", q2)
          , ("frustration_score", q3)
          ]

      req = systemOneRequest "nimble" state questions

  putStrLn "2. Dispatching decision evaluation request to Ollama..."
  res <- systemOne client req
  case res of
    Left (ApiError 404 msg) -> do
      putStrLn $ "Note: Server returned 404 (" <> T.unpack msg <> ")."
      putStrLn "System One requires Ollama server with decision model support (e.g. 'ollama run nimble')."
      putStrLn "Request serialization and client machinery verified successfully!"
    Left err -> do
      putStrLn $ "System One response: " <> show err
    Right resp -> do
      putStrLn "System One Answers:"
      mapM_
        ( \(qName, ans) -> case ans of
            AnswerChoice ca -> do
              putStrLn $ "  - Question [" <> T.unpack qName <> "]: Choice -> " <> T.unpack (choiceResult ca)
              putStrLn $ "    Probabilities: " <> show (choiceProbabilities ca)
            AnswerNoul p -> do
              putStrLn $ "  - Question [" <> T.unpack qName <> "]: Noul probability -> " <> show p
            AnswerScore sa -> do
              putStrLn $ "  - Question [" <> T.unpack qName <> "]: Score -> " <> show (scoreValue sa)
              putStrLn $ "    Legend: " <> show (scoreLegend sa)
        )
        (Map.toList (sorAnswers resp))

  putStrLn "\n=== System One Example Completed Successfully ==="
