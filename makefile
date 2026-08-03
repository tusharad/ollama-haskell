.PHONY: build test check lint format format-check docs clean help

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
	fourmolu --check src/ test/

## Build Haddock documentation
docs:
	cabal haddock

## Clean build artifacts
clean:
	cabal clean

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
	@echo "  clean         Remove dist-newstyle build directory"
