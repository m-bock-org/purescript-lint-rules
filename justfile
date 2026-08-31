export PATH := justfile_directory() / "node_modules/.bin:" + env_var('PATH')
set shell := ["bash", "-c"]

build:
    spago build --strict

format:
    purs-tidy format-in-place 'src/**/*.purs'

lint:
    spago test -m Test.Lint

check: build lint
