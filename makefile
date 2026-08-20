.PHONY: build test check lint format format-check docs docs-site docs-serve docs-clean clean help

.DEFAULT_GOAL := help

## Build the project
build:
	cabal build

## Run test suite
test:
	cabal test

## Run cabal check
check:
	cabal check

## Run HLint linter
lint:
	hlint src/ test/

## Format source code with Fourmolu
format:
	fourmolu -i src/ test/

## Check code formatting with Fourmolu
format-check:
	fourmolu -m check src/ test/

## Build Haddock documentation
docs:
	cabal haddock

## Build static Hakyll documentation website
docs-site:
	cd docs && cabal run site -- clean && cabal run site -- build

## Serve Hakyll documentation website locally with live reload
docs-serve:
	cd docs && cabal run site -- watch

## Clean Hakyll site cache and generated output
docs-clean:
	cd docs && cabal run site -- clean

## Clean build artifacts
clean:
	cabal clean
	cd docs && cabal run site -- clean || true

e2e:
	cabal run ollama-haskell-integration --flags="+integration-tests"

## Display help message
help:
	@echo "Available Makefile targets:"
	@echo "  build         Build library and executables"
	@echo "  test          Run unit and integration test suites"
	@echo "  check         Run cabal check"
	@echo "  lint          Run hlint static analysis"
	@echo "  format        Format Haskell source files with fourmolu"
	@echo "  format-check  Verify code formatting with fourmolu"
	@echo "  docs          Generate Haddock documentation"
	@echo "  docs-site     Generate static Hakyll documentation site"
	@echo "  docs-serve    Run local Hakyll preview server (http://127.0.0.1:8000)"
	@echo "  docs-clean    Clean Hakyll build cache and output"
	@echo "  clean         Remove dist-newstyle build directory"
