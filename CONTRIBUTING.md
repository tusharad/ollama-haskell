# Contributing to ollama-haskell

Thank you for considering contributing to `ollama-haskell`!

---

## 1. Prerequisites

- **GHC:** 9.4+ (GHC 9.6 recommended)
- **Cabal:** 3.0+
- **Tools:** `fourmolu` and `hlint` installed locally
- **Ollama Server:** Running locally on `http://127.0.0.1:11434` for integration testing

---

## 2. Development Setup

```bash
# Clone the repository
git clone https://github.com/tusharad/ollama-haskell.git
cd ollama-haskell

# Build library and tests
make build

# Run unit and golden test suites
make test

# Check Cabal package specification
make check
```

---

## 3. Code Standards & Guidelines

1. **Language Standard:** Code uses `GHC2021` with `StrictData` enabled globally.
2. **Formatting:** Code must be formatted with `fourmolu`. Run `make format` before committing.
3. **Linting:** Code must pass `hlint` with zero warnings. Run `make lint`.
4. **No Partial Functions:** Do not use `head`, `tail`, `fromJust`, `read`, `error`, or `undefined`.
5. **MonadIO Polymorphism:** All API functions must use `MonadIO m =>` or `MonadUnliftIO m =>`.
6. **Documentation:** Add Haddock comments to all exported types and functions, including `@since 3.0.0.0`.

---

## 4. Pull Request Process

1. Create a feature branch (`git checkout -b my-feature`).
2. Implement your changes following the design patterns in [TDD.md](./TDD.md).
3. Ensure `make check`, `make build`, `make test`, `make lint`, and `make format-check` all pass.
4. Submit a Pull Request targeting `main`.
