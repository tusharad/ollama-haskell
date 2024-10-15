# Ollama-haskell

**ollama-haskell** is an unofficial Haskell binding for [Ollama](https://ollama.com), similar to [`ollama-python`](https://github.com/ollama/ollama-python). 

This library allows you to interact with Ollama, a tool that lets you run large language models (LLMs) locally, from within your Haskell projects. 

## Examples

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Ollama

main :: IO ()
main = do
    generate "llama3.1" "Is haskell a good language?"
```

### Output

```bash
ghci> import Lib
ghci> main

Whether Haskell is a "good" language depends on what you're looking for in a programming language and your personal preferences. Here are some points to consider:

**Pros:**

1. **Strongly typed**: Haskell's type system ensures that you catch errors early, which leads to fewer bugs and easier maintenance.
2. **Functional programming paradigm**: Haskell encourages declarative coding, making it easier to reason about code and write correct programs.
3. **Garbage collection**: Haskell handles memory management automatically, freeing you from worries about manual memory deallocation.
```

## Prerequisite

Make sure you have [Ollama](https://ollama.com) installed and running on your local machine. You can download it from [here](https://ollama.com/download).

## How to Use It

1. Add this GitHub repository link to your project's dependencies in the `stack.yaml` file:
   ```yaml
   extra-deps:
     - git: https://github.com/tusharad/ollama-haskell
       commit: <latest-commit-hash>
   ```
2. Include the `ollama-haskell` package in your `.cabal` file:
   ```cabal
   build-depends:
       base >= 4.7 && < 5,
       ollama-haskell
   ```

3. Import the `Ollama` module and start integrating with your local LLM.

## Future Updates

- [x] Improve documentation
- [x] Add tests.
- [ ] Add examples.
- [ ] Add CI/CD pipeline.
- [ ] `options` parameter in `generate`.

Stay tuned for future updates and improvements!

## Author

This library is developed and maintained by [Tushar](https://github.com/tusharad). Feel free to reach out for any questions or suggestions!

## Contributions

Contributions are welcome! If you'd like to improve the library, please submit a pull request or open an issue. Whether it's fixing bugs, adding new features, or improving documentation, all contributions are greatly appreciated.
