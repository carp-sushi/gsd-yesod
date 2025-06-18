.PHONY: all format build test lint run clean watch

all: format build

format:
	@fourmolu -q -i src/{Application,Database,Foundation,Handler,Model,Page,Settings}.hs app/Main.hs

build:
	@stack build

test:
	@stack test

lint:
	@hlint src/*.hs app/*.hs

run:
	@stack run -- config/settings_dev

clean:
	@stack purge
	@rm -rf dist-newstyle

watch:
	ghciwatch --clear --before-reload-shell "make format"
