ghcid: dev
dev: ## Start ghcid
	@ghcid --target lib:quad --allow-eval --warnings

start-server: ## Start the server
	@cabal run exe:quad -- start-server

start-agent: ## Start the agent
	@cabal run exe:quad -- start-agent

deps: ## Install the dependencies of the backend
	@cabal build --only-dependencies

build: ## Build the project in fast mode
	@cabal build -O0

clean: ## Remove compilation artifacts
	@cabal clean

frontend-deps: ## Install the dependencies of the frontend
	@cd frontend/ && yarn

frontend-run: ## Run the frontend
	@cd frontend/ && yarn next

frontend-clean: ## Remove JS artifacts
	@cd frontend/ && rm -R node_modules

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

format: style
style: ## Run the code styler (stylish-haskell)
	@stylish-haskell -i -r src app test

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
