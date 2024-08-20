## Haskell bindings for Ollama.

This is going to be a library.
The interface will look, something like this:

This library provides a API design around this [Doc](https://github.com/ollama/ollama/blob/main/docs/api.md).
The project is inspired by Ollama's [python](https://github.com/ollama/ollama-python) library.

Ollama.hs
Data
  Ollama
    Internal.hs

###A rough set of APIs:

Need to create a client called Ollama or something. Which will contain stuff like host,timeout, headers.

- chat :: Model -> Messages ([Text])  -- Main function
- generate
- list
- show :: Model
- create
- copy :: Model -> Model -> 
- Delete :: Model
- Pull :: Model
- Embeddings :: Model -> Prompt
- Ps

