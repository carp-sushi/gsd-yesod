.PHONY: all format build test lint run clean watch

all: format build test

format:
	@fourmolu -q -i \
		app/Main.hs \
		src/Application.hs \
		src/Database.hs \
		src/Dto.hs \
		src/Foundation.hs \
		src/Handler.hs \
		src/Logger.hs \
		src/Model.hs \
		src/Page.hs \
		src/Settings.hs

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
