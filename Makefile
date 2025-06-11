.PHONY: all format build lint run clean watch

all: format build

format:
	@fourmolu -q -i src/*.hs app/*.hs

build:
	@stack build

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run -- config/settings

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear --before-reload-shell "make format"
