# Ollama Haskell Examples

This project provides example Haskell code for interacting with [Ollama](https://ollama.com) using its HTTP API. These examples demonstrate how to chat with models, generate text, use structured output, handle multimodal input, and more — all from Haskell.

> **Note**: These examples require that the Ollama server is running locally. See [Ollama API Docs](https://github.com/ollama/ollama/blob/main/docs/api.md) for full API reference.

## ▶️ Running Examples

You can run individual examples using `stack` or `cabal`. For example:

```sh
stack run
```

## ✨ Examples Overview

### 💬 Chat - Chat with a model

* [Basic chat example](./src/Example/SimpleChat.hs)
* [Chat with multi-turn conversation](./src/Example/ChatConversation.hs)
* [Chat using function/tool call](./src/Example/ChatWithToolCall.hs)

### 📝 Generate - Generate text with a model

* [Simple text generation](./src/Example/SimpleGenerate.hs)
* [Streamed text generation](./src/Example/GenerateStream.hs)
* [Text generation with thinking mode](./src/Example/GenerateThinking.hs)
* [Text generation with custom config](./src/Example/GenerateWithConfig.hs)
* [Text generation with model options](./src/Example/GenerateWithModelOptions.hs)

### 🧰 Structured Outputs

* [Structured output from chat](./src/Example/ChatStructuredOutput.hs)
* [Structured output including image](./src/Example/ChatStructuredOutputImage.hs)

### 🖼️ Multimodal - Chat with images

* [Chat with image input](./src/Example/ChatWithImage.hs)

### 🧠 Embeddings

* [Generate embeddings](./src/Example/Embeddings.hs)

### 📚 Knowledge Integration

* [Knowledge-based question answering](./src/Example/KnowledgeApp.hs)

### 📋 Model Management

* [List available models](./src/Example/List.hs)

## 🛠 Requirements

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) or Cabal
* [Ollama](https://ollama.com/) installed and running locally

## 📄 License

This project is licensed under the terms of the [MIT License](LICENSE).
