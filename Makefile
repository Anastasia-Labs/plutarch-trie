
.PHONY: help
help:
	@echo "use: make <command>"
	@echo
	@echo "Where <command>:"
	@echo "  help       -- show this help"
	@echo "  shell      -- opens a development shell for the smart contract project"
	@echo "  clean      -- cabal clean"
	@echo "  build      -- build the smart contracts"
	@echo "  test       -- run the smart contract tests"
	@echo "  ci         -- run ci job"

.PHONY: shell
shell:
	nix develop --accept-flake-config --extra-experimental-features ca-derivations

.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test -f development --test-show-details=direct

.PHONY: ci
ci:
	nix flake check --accept-flake-config --extra-experimental-features ca-derivations
