# 🦙 Ollama Haskell

<div style="text-align: center;">
<img src="./examples/ollama_haskell.png" alt="logo image" height="200"/>
</div>

**`ollama-haskell`** is an unofficial Haskell client for [Ollama](https://ollama.com), inspired by [`ollama-python`](https://github.com/ollama/ollama-python). It enables interaction with locally running LLMs through the Ollama HTTP API — directly from Haskell.

---

## ✨ Features

* 💬 Chat with models
* ✍️ Text generation (with streaming)
* ✅ Chat with structured messages and tools
* 🧠 Embeddings
* 🧰 Model management (list, pull, push, show, delete)
* 🗃️ In-memory conversation history
* ⚙️ Configurable timeouts, retries, streaming handlers

---

## ⚡ Quick Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Ollama.Generate
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let ops =
        defaultGenerateOps
          { modelName = "gemma3"
          , prompt = "What is the meaning of life?"
          }
  eRes <- generate ops Nothing
  case eRes of
    Left err -> putStrLn $ "Something went wrong: " ++ show err
    Right r -> do
      putStr "LLM response: "
      T.putStrLn (genResponse r)
```

---

## 📦 Installation

Add to your `.cabal` file:

```cabal
build-depends:
  base >=4.7 && <5,
  ollama-haskell
```

Or use with `stack`/`nix-shell`.

---

## 📚 More Examples

See [`examples/OllamaExamples.hs`](examples/OllamaExamples.hs) for:

* Chat with conversation memory
* Structured JSON output
* Embeddings
* Tool/function calling
* Multimodal input
* Streaming and non-streaming variants

---

## 🛠 Prerequisite

Make sure you have [Ollama installed and running locally](https://ollama.com/download). Run `ollama pull llama3` to download a model.

---

## 🧪 Dev & Nix Support

Use Nix:

```bash
nix-shell
```

This will install `stack` and Ollama.

---

## 👨‍💻 Author

Created and maintained by [@tusharad](https://github.com/tusharad). PRs and feedback are welcome!

---

## 🤝 Contributing

Have ideas or improvements? Feel free to [open an issue](https://github.com/tusharad/ollama-haskell/issues) or submit a PR!
