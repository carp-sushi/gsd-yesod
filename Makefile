.PHONY: all format build test lint run clean watch

all: format build test lint

# The formatter mangles esqueleto queries, so format all code except those.
HS_FILES = $(filter-out src/Query.hs, $(shell find app src -name "*.hs"))

format:
	@fourmolu -q -i $(HS_FILES)

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run -- config/dev/settings

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear --before-reload-shell "make format"
