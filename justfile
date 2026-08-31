export PATH := justfile_directory() / "node_modules/.bin:" + env_var('PATH')
set shell := ["bash", "-c"]

build:
    spago build

# The gate's build: warnings are errors, and the wipe is what makes the
# count real - purs does not re-report a warning for a module it did not
# recompile, so an incremental strict build can print zero while warnings
# genuinely exist. Same thing CI does from a fresh checkout.
strict:
    rm -rf output
    spago build --strict

format:
    purs-tidy format-in-place 'src/**/*.purs'

docs:
    spago test -m Test.Docs

# fails if the README's rules table is out of date with the rules
docs-check:
    #!/usr/bin/env bash
    set -euo pipefail
    before=$(mktemp)
    cp README.md "$before"
    spago test -m Test.Docs >/dev/null
    diff -u "$before" README.md

lint:
    spago test -m Test.Lint

check: strict lint docs-check
