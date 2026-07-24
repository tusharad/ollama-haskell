# Contributing to ollama-haskell

Thank you for considering contributing to `ollama-haskell`!

---

## Development Setup

### Prerequisites
- **GHC**: 9.2, 9.4, or 9.6+
- **Cabal**: 3.6+
- **Ollama Server**: Optional for running integration tests against a real local server (`http://127.0.0.1:11434`).

### Building the Project

Clone the repository and build using Cabal:

```bash
git clone https://github.com/tusharadhatrao/ollama-haskell.git
cd ollama-haskell
cabal build
```

---

## Running Tests

### Pure Unit & Property Test Suite (No server required)

Run the fast pure test suite:

```bash
cabal test
```

This runs:
- Smart constructor validation tests.
- `SchemaBuilder` DSL tests.
- QuickCheck property roundtrip tests.
- Golden JSON wire format tests.

### Live Server Integration Test Suite

To run integration tests against a running Ollama server:

```bash
cabal test ollama-haskell-integration --flag=integration-tests
```

---

## Quality & Verification Commands

Before submitting a Pull Request, verify all quality checks pass:

```bash
# Package compliance check
cabal check

# Build documentation cleanly
cabal haddock

# Verify source distribution tarball
cabal sdist
```

---

## Code Style

- Format Haskell source code using `fourmolu`.
- Include explicit export lists on every module.
- Add Haddock documentation comments (`{- | ... -}`) for all exported functions and data types with `@since 1.0.0.0` annotations.
